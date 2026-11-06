(in-package #:lisp-source-protocol)

;;; A lexical paren scanner, independent of the reader. It is only used to
;;; explain broken input: where an opener lost its closer, and where an
;;; indentation-based guess puts the missing or extra delimiter.

(defstruct (paren-event (:constructor make-paren-event (type offset line column)))
  type offset line column)

(defun scan-parens (text start end)
  "Return a vector of :OPEN / :CLOSE paren events between START and END.
Skips strings, line and block comments, character literals and |escaped| tokens."
  (let ((s (source-string text))
        (events (make-array 16 :adjustable t :fill-pointer 0))
        (i start))
    (flet ((emit (type)
             (multiple-value-bind (line col) (offset-line-column text i)
               (vector-push-extend (make-paren-event type i line col) events))))
      (loop while (< i end)
            do (let ((c (char s i)))
                 (case c
                   (#\( (emit :open) (incf i))
                   (#\) (emit :close) (incf i))
                   (#\; (loop while (and (< i end) (char/= (char s i) #\Newline))
                              do (incf i)))
                   (#\" (incf i)
                    (loop while (< i end)
                          do (let ((d (char s i)))
                               (incf i)
                               (cond ((char= d #\\) (incf i))
                                     ((char= d #\") (return))))))
                   (#\| (incf i)
                    (loop while (< i end)
                          do (let ((d (char s i)))
                               (incf i)
                               (cond ((char= d #\\) (incf i))
                                     ((char= d #\|) (return))))))
                   (#\\ (incf i 2))
                   (#\# (let ((d (and (< (1+ i) end) (char s (1+ i)))))
                          (case d
                            (#\\ (incf i 3)
                             ;; character names like #\Space
                             (loop while (and (< i end) (alphanumericp (char s i)))
                                   do (incf i)))
                            (#\| (incf i 2)
                             (let ((depth 1))
                               (loop while (and (< i end) (plusp depth))
                                     do (cond ((and (char= (char s i) #\|) (< (1+ i) end)
                                                    (char= (char s (1+ i)) #\#))
                                               (decf depth) (incf i 2))
                                              ((and (char= (char s i) #\#) (< (1+ i) end)
                                                    (char= (char s (1+ i)) #\|))
                                               (incf depth) (incf i 2))
                                              (t (incf i))))))
                            (t (incf i)))))
                   (t (incf i)))))
      events)))

(defun line-indentation (text line)
  "Column of the first non-blank character of 1-based LINE, or NIL when the
line is blank or a comment."
  (let* ((s (source-string text))
         (start (line-start-offset text line))
         (end (line-end-offset text line)))
    (loop for i from start below end
          for c = (char s i)
          unless (member c '(#\Space #\Tab #\Return))
            do (return (if (char= c #\;) nil (- i start))))))

(defun last-code-offset-before-line (text line floor)
  "Offset just after the last non-blank, non-comment character on a line
before 1-based LINE, not earlier than FLOOR."
  (let ((s (source-string text)))
    (loop for l from (1- line) downto 1
          for start = (line-start-offset text l)
          when (< (line-end-offset text l) floor) do (return floor)
          do (let ((code-end nil))
               ;; scan the line, dropping a trailing comment (approximate: first ; outside strings)
               (let ((in-string nil)
                     (i start)
                     (stop (line-end-offset text l)))
                 (loop while (< i stop)
                       do (let ((c (char s i)))
                            (cond (in-string (cond ((char= c #\\) (incf i))
                                                   ((char= c #\") (setf in-string nil)))
                                             (setf code-end (min stop (1+ i))))
                                  ((char= c #\") (setf in-string t code-end (1+ i)))
                                  ((char= c #\;) (return))
                                  ((not (member c '(#\Space #\Tab #\Return)))
                                   (setf code-end (1+ i))))
                            (incf i))))
               (when code-end (return (max code-end floor)))))))

(defun infer-missing-close (text form-start end)
  "Explain a form starting at FORM-START that is still open at END.
Return (values count innermost-open fix-position confidence)."
  (let ((events (scan-parens text form-start end))
        (stack '())
        (first-dedent nil))
    (loop for e across events
          do (ecase (paren-event-type e)
               (:open (push e stack))
               (:close (pop stack))))
    ;; Indentation pass: a code line indented at or left of an open paren
    ;; that opened on an earlier line means that paren should have closed
    ;; before this line.
    (let ((open '())
          (ei 0)
          (n (length events)))
      (multiple-value-bind (first-line) (offset-line-column text form-start)
        (multiple-value-bind (last-line) (offset-line-column text (max form-start (1- end)))
          (loop for line from first-line to last-line
                for indent = (line-indentation text line)
                do (when (and indent (> line first-line) (null first-dedent))
                     (let ((victim (find-if (lambda (o) (>= (paren-event-column o) indent)) open)))
                       (when victim
                         (setf first-dedent
                               (cons victim (last-code-offset-before-line
                                             text line (paren-event-offset victim)))))))
                   (loop while (and (< ei n) (= (paren-event-line (aref events ei)) line))
                         do (let ((e (aref events ei)))
                              (ecase (paren-event-type e)
                                (:open (push e open))
                                (:close (pop open))))
                            (incf ei))))))
    (let ((count (max 1 (length stack)))
          (innermost (and stack (paren-event-offset (first stack)))))
      (if first-dedent
          (values count (paren-event-offset (car first-dedent)) (cdr first-dedent) :high)
          (values count innermost
                  (multiple-value-bind (line col) (offset-line-column text end)
                    ;; END is exclusive: a cut sits at a line start.
                    (last-code-offset-before-line text (if (zerop col) line (1+ line))
                                                  form-start))
                  :medium)))))

(defun infer-extra-close (text form-start form-end)
  "Find a closer inside [FORM-START, FORM-END) that indentation says came too
early: a code line indented right of an opener that the previous code line
closed, while the still-open parent sits further left. Return the closer's
offset or NIL."
  (let ((events (scan-parens text form-start form-end))
        (open '())
        (prev-closed '())
        (ei 0))
    (multiple-value-bind (first-line) (offset-line-column text form-start)
      (multiple-value-bind (last-line) (offset-line-column text (max form-start (1- form-end)))
        (loop for line from first-line to last-line
              for indent = (line-indentation text line)
              do (when (and indent (> line first-line))
                   (let* ((parent-col (if open (paren-event-column (first open)) -1))
                          (victim (loop for (closer . opener) in prev-closed
                                        when (< parent-col (paren-event-column opener) indent)
                                          maximize (paren-event-column opener) into best
                                          and collect (cons (paren-event-column opener) closer)
                                                into hits
                                        finally (return (cdr (assoc best hits))))))
                     (when victim (return-from infer-extra-close (paren-event-offset victim))))
                   (setf prev-closed '()))
                 (loop while (and (< ei (length events))
                                  (= (paren-event-line (aref events ei)) line))
                       do (let ((e (aref events ei)))
                            (ecase (paren-event-type e)
                              (:open (push e open))
                              (:close (let ((opener (pop open)))
                                        (when opener (push (cons e opener) prev-closed))))))
                          (incf ei)))))
    nil))
