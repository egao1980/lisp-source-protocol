(defpackage #:lisp-source-protocol/tests
  (:use #:cl #:rove #:lisp-source-protocol))

(in-package #:lisp-source-protocol/tests)

(defun kinds (ps)
  (mapcar #'node-kind (parsed-source-nodes ps)))

(defun texts (ps)
  (mapcar #'node-text (parsed-source-forms ps)))

(defun lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(defun fixture-path (name)
  (asdf:system-relative-pathname "lisp-source-protocol" (format nil "tests/fixtures/~A" name)))

(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun first-uncovered (ps)
  "Offset of the first non-whitespace character no top-level node covers."
  (let* ((s (source-string (parsed-source-text ps)))
         (mark (make-array (length s) :element-type 'bit :initial-element 0)))
    (dolist (n (parsed-source-nodes ps))
      (fill mark 1 :start (node-start n) :end (node-end n)))
    (loop for i below (length s)
          when (and (zerop (aref mark i)) (not (whitespace-char-p (char s i))))
            return i)))
