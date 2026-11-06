(in-package #:lisp-source-protocol)

;;; Locators
;;;
;;;   (:path 3 0 2)                         structural path from the root
;;;   (:offset 120)                         innermost expression containing offset
;;;   (:line 12 :column 4)                  same, by 1-based line / 0-based column
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
  "Top-level forms (also inside top-level PROGN / EVAL-WHEN) defined by OPERATOR naming NAME."
  (let ((matches '()))
    (labels ((visit (form)
               (let ((head (node-head-name form)))
                 (cond ((null head))
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

(defun locate (parsed locator)
  "Resolve LOCATOR to exactly one node. Signals LOCATOR-NOT-FOUND, or
AMBIGUOUS-LOCATOR with a USE-VALUE restart taking one of the candidates."
  (ecase (first locator)
    (:path (node-at-path parsed (rest locator)))
    (:offset (node-at-offset parsed (second locator)))
    (:line (destructuring-bind (line &key (column 0)) (rest locator)
             (let ((text (parsed-source-text parsed)))
               (unless (<= 1 line (line-count text))
                 (error 'locator-not-found :locator locator))
               (let ((start (line-start-offset text line)))
                 (node-at-offset parsed (or (position-if-not
                                             (lambda (c) (member c '(#\Space #\Tab)))
                                             (source-string text)
                                             :start (+ start column)
                                             :end (line-end-offset text line))
                                            (+ start column)))))))
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
