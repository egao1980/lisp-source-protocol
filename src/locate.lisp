(in-package #:lisp-source-protocol)

;;; Locators
;;;
;;;   (:path 3 0 2)                         structural path from the root
;;;   (:offset 120)                         innermost expression containing offset
;;;   (:line 12 :column 4)                  same, by 1-based line / 0-based column;
;;;                                         a top-level comment there: its block
;;;   (:comment 12)  (:comment 12 14)       comment block starting on line 12 (to 14)
;;;   (:definition "defun" "foo")           top-level definition by operator + name
;;;   (:definition "defmethod" "print-object" "(my-class t)")  with specializers
;;;   (:hash "…")                           top-level form whose text hash matches

(defun node-at-path (parsed path)
  (let ((node (parsed-source-root parsed)))
    (dolist (i path node)
      (let ((forms (node-forms node)))
        (unless (and (integerp i) (< -1 i (length forms)))
          (error 'locator-not-found :locator (cons :path path)))
        (setf node (nth i forms))))))

(defun node-at-offset (parsed offset)
  "Innermost expression node whose range contains OFFSET."
  (let ((best nil))
    (labels ((visit (n)
               (when (and (<= (node-start n) offset) (< offset (node-end n)))
                 (when (expression-node-p n) (setf best n))
                 (mapc #'visit (node-children n)))))
      (mapc #'visit (parsed-source-nodes parsed)))
    (or best (error 'locator-not-found :locator (list :offset offset)))))

(defun normalize-space (string)
  (let ((out (make-string-output-stream))
        (space nil))
    (loop for c across (string-trim '(#\Space #\Tab #\Newline #\Return) string)
          do (if (member c '(#\Space #\Tab #\Newline #\Return))
                 (setf space t)
                 (progn (when space (write-char #\Space out) (setf space nil))
                        (write-char c out))))
    (string-downcase (get-output-stream-string out))))

(defun definition-name-node (form)
  "Name node of a definition form: second element, unwrapping (setf foo)."
  (second (node-forms form)))

(defun definition-name-string (form)
  (let ((name (definition-name-node form)))
    (when name
      (if (eq (node-kind name) :symbol)
          (symbol-token-name (node-value name))
          (normalize-space (node-text name))))))

(defun method-specializers-string (form)
  "Text of the first list after the name and qualifiers of a DEFMETHOD."
  (let ((lambda-list (find :list (cddr (node-forms form)) :key #'node-kind)))
    (when lambda-list
      (normalize-space
       (format nil "(~{~A~^ ~})"
               (loop for arg in (node-forms lambda-list)
                     for name = (node-symbol-name arg)
                     until (and name (char= (char name 0) #\&))
                     collect (if (eq (node-kind arg) :list)
                                 (let ((spec (second (node-forms arg))))
                                   (if spec (node-text spec) "t"))
                                 "t")))))))

(defun find-definition-nodes (parsed operator name &optional specializers)
  "Top-level forms (also inside top-level PROGN / EVAL-WHEN and #+/#- branches,
included or not) defined by OPERATOR naming NAME."
  (let ((matches '()))
    (labels ((visit (form)
               (let ((head (node-head-name form)))
                 (cond ((eq (node-kind form) :feature-conditional)
                        (mapc #'visit (feature-conditional-forms form)))
                       ((null head))
                       ((member head '("PROGN" "EVAL-WHEN" "LOCALLY") :test #'string=)
                        (mapc #'visit (cdr (node-forms form))))
                       ((and (string-equal head operator)
                             (let ((n (definition-name-string form)))
                               (and n (or (string-equal n name)
                                          (string= n (normalize-space name)))))
                             (or (null specializers)
                                 (equal (method-specializers-string form)
                                        (normalize-space specializers))))
                        (push form matches))))))
      (mapc #'visit (parsed-source-forms parsed)))
    (nreverse matches)))

;;; Comment blocks

(defun comment-node-p (node)
  (member (node-kind node) '(:comment :block-comment)))

(defun comment-text-end (node)
  "End of comment NODE's text: before the newline a ; comment ends with."
  (let ((end (node-end node)))
    (if (and (> end (node-start node))
             (char= (char (source-string (node-source node)) (1- end)) #\Newline))
        (1- end)
        end)))

(defun blanks-only-p (s start end)
  (every (lambda (c) (member c '(#\Space #\Tab #\Return))) (subseq s start end)))

(defun own-lines-comment-p (node)
  "True when comment NODE has only blanks before it and after it on its lines,
and sits in a list, a vector or the top level."
  (let* ((s (source-string (node-source node)))
         (start (node-start node))
         (end (comment-text-end node))
         (parent (node-parent node)))
    (and (comment-node-p node)
         parent (member (node-kind parent) '(:root :list :vector))
         (blanks-only-p s (1+ (or (position #\Newline s :end start :from-end t) -1)) start)
         (blanks-only-p s end (or (position #\Newline s :start end) (length s))))))

(defun comment-block-from (node &optional last-line)
  "The comment block whose first comment is NODE (OWN-LINES-COMMENT-P): NODE
and the own-line comments after it, with one line break between each, up to
those starting on LAST-LINE when given."
  (let* ((text (node-source node))
         (s (source-string text))
         (members (list node)))
    (loop for next in (rest (member node (node-children (node-parent node))))
          for prev = (first members)
          while (and (comment-node-p next) (own-lines-comment-p next)
                     (= 1 (count #\Newline s :start (comment-text-end prev) :end (node-start next)))
                     (or (null last-line)
                         (<= (offset-line-column text (node-start next)) last-line)))
          do (push next members))
    (let ((block (make-instance 'comment-block-node
                                :kind :comment-block :start (node-start node)
                                :end (comment-text-end (first members))
                                :children (reverse members) :source text)))
      (setf (node-parent block) (node-parent node))
      block)))

(defun line-comment-node (parsed line &key top-level-only)
  "The own-line comment whose text starts LINE (1-based), or NIL. Only a
top-level one when TOP-LEVEL-ONLY."
  (let ((text (parsed-source-text parsed)))
    (when (<= 1 line (line-count text))
      (let* ((s (source-string text))
             (start (line-start-offset text line))
             (pos (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s
                                   :start start :end (line-end-offset text line))))
        (when pos
          (if top-level-only
              (find-if (lambda (n) (and (= (node-start n) pos) (own-lines-comment-p n)))
                       (parsed-source-nodes parsed))
              (block found
                (walk-nodes (lambda (n)
                              (when (and (= (node-start n) pos) (own-lines-comment-p n))
                                (return-from found n)))
                            (parsed-source-nodes parsed))
                nil)))))))

(defun comment-block-at (parsed line &optional last-line)
  "The comment block starting on LINE (1-based) of PARSED, top level or inside
a list, through LAST-LINE when given. Signals LOCATOR-NOT-FOUND when no
own-line comment starts LINE."
  (let ((node (and (or (null last-line) (<= line last-line))
                   (line-comment-node parsed line))))
    (unless node
      (error 'locator-not-found :locator (list* :comment line (and last-line (list last-line)))))
    (comment-block-from node last-line)))

(defun comment-blocks (parsed)
  "The top-level comment blocks of PARSED, in order, each as long as it goes."
  (let ((blocks '())
        (covered nil))
    (dolist (n (parsed-source-nodes parsed))
      (when (and (comment-node-p n) (not (member n covered)) (own-lines-comment-p n))
        (let ((block (comment-block-from n)))
          (setf covered (node-children block))
          (push block blocks))))
    (nreverse blocks)))

(defun locate (parsed locator)
  "Resolve LOCATOR to exactly one node. Signals LOCATOR-NOT-FOUND, or
AMBIGUOUS-LOCATOR with a USE-VALUE restart taking one of the candidates.
(:LINE N) on a line that a top-level comment starts gives the comment block
starting there, like (:COMMENT N)."
  (ecase (first locator)
    (:path (node-at-path parsed (rest locator)))
    (:offset (node-at-offset parsed (second locator)))
    (:comment (destructuring-bind (line &optional last-line) (rest locator)
                (comment-block-at parsed line last-line)))
    (:line (destructuring-bind (line &key (column 0)) (rest locator)
             (let ((text (parsed-source-text parsed)))
               (unless (<= 1 line (line-count text))
                 (error 'locator-not-found :locator locator))
               (let* ((start (line-start-offset text line))
                      (offset (or (position-if-not
                                   (lambda (c) (member c '(#\Space #\Tab)))
                                   (source-string text)
                                   :start (+ start column)
                                   :end (line-end-offset text line))
                                  (+ start column)))
                      (comment (line-comment-node parsed line :top-level-only t)))
                 (if (and comment (<= (node-start comment) offset) (< offset (node-end comment)))
                     (comment-block-from comment)
                     (node-at-offset parsed offset))))))
    ((:definition :hash)
     (let ((candidates
             (if (eq (first locator) :hash)
                 (remove (second locator) (parsed-source-forms parsed)
                         :key #'node-hash :test-not #'string=)
                 (destructuring-bind (operator name &optional specializers) (rest locator)
                   (find-definition-nodes parsed operator name specializers)))))
       (cond ((null candidates) (error 'locator-not-found :locator locator))
             ((rest candidates)
              (restart-case (error 'ambiguous-locator :locator locator :candidates candidates)
                (use-value (node)
                  :report "Use one of the candidate nodes"
                  node)))
             (t (first candidates)))))))
