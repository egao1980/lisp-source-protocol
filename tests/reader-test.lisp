(in-package #:lisp-source-protocol/tests)

(deftest-parametrize expression-kinds
    ((source kind)
     ("(a b)" :list)
     ("'x" :quote)
     ("`(a ,b)" :quasiquote)
     ("#'car" :function)
     ("#(1 2)" :vector)
     ("#\\a" :character)
     ("\"s\"" :string)
     ("foo" :symbol)
     ("#:foo" :symbol)
     ("pkg::foo" :symbol)
     ("42" :number)
     ("#x1F" :dispatch)
     ("#+sbcl x" :feature-conditional)
     ("#1=(a)" :label-definition))
  (let ((ps (read-source source :features '(:sbcl))))
    (ok (parsed-source-clean-p ps))
    (ok (eq (node-kind (first (parsed-source-forms ps))) kind))))

(deftest-parametrize trivia-kinds
    ((source kind)
     ("; c" :comment)
     ("#| b |#" :block-comment)
     ("#+(or) (x)" :feature-skipped)
     ("#-sbcl (x)" :feature-skipped))
  (let ((ps (read-source source :features '(:sbcl))))
    (ok (equal (kinds ps) (list kind)))
    (ok (null (parsed-source-forms ps)))))

(deftest symbols-are-tokens-not-interned
  (let* ((name "LISP-SOURCE-PROTOCOL-TEST-NEVER-INTERNED")
         (ps (read-source (format nil "(no-such-package:~A ~:*~A)" name)))
         (forms (node-forms (first (parsed-source-forms ps)))))
    (ok (parsed-source-clean-p ps))
    (ok (null (find-symbol name :cl-user)))
    (ok (equal (symbol-token-package (node-value (first forms))) "NO-SUCH-PACKAGE"))
    (ok (eq (symbol-token-package (node-value (second forms))) :current))))

(deftest read-eval-is-never-evaluated
  (let ((signalled 0))
    (handler-bind ((read-time-eval-refused (lambda (c) (declare (ignore c)) (incf signalled))))
      (let ((ps (read-source "(a #.(error \"boom\") b)")))
        (ok (= signalled 1))
        (ok (= 3 (length (node-forms (first (parsed-source-forms ps))))))
        (ok (find :read-time-eval (parsed-source-diagnostics ps) :key #'diagnostic-kind))
        (ok (parsed-source-clean-p ps) "a #. warning does not make the file unclean")))))

(deftest sharp-s-is-not-constructed
  (let ((ps (read-source "#S(no-such-struct :a 1)")))
    (ok (parsed-source-clean-p ps))
    (ok (eq :dispatch (node-kind (first (parsed-source-forms ps)))))))

(deftest-parametrize implementation-character-names
    ((source)
     ("#\\Nul")
     ("#\\Esc")
     ("#\\uFEFF")
     ("#\\Space"))
  (ok (parsed-source-clean-p (read-source source))))

(deftest comments-inside-forms-are-children
  (let* ((ps (read-source (lines "(defun f ()" "  ;; note" "  1)")))
         (f (first (parsed-source-forms ps))))
    (ok (find :comment (node-children f) :key #'node-kind))
    (ok (= 4 (length (node-forms f))) "defun f () 1")))

(deftest paths-and-parents
  (let* ((ps (read-source "(a (b c)) (d)"))
         (c (node-at-path ps '(0 1 1))))
    (ok (string= (node-text c) "c"))
    (ok (equal (node-path c) '(0 1 1)))
    (ok (string= (node-text (node-parent c)) "(b c)"))))

(deftest outline-entries
  (let* ((ps (read-source (lines "(in-package :foo)"
                                 "(defun f (a &key b) \"Doc.\" a)"
                                 "(defmethod g ((x foo) y) x)"
                                 "(defclass k () () (:documentation \"K\"))"
                                 "(eval-when (:compile-toplevel) (defvar *v* 1 \"V\"))")))
         (entries (outline ps)))
    (ok (equal (mapcar #'outline-entry-kind entries)
               '("in-package" "defun" "defmethod" "defclass" "defvar")))
    (ok (equal (mapcar #'outline-entry-name entries) '("FOO" "F" "G" "K" "*V*")))
    (ok (equal (mapcar #'outline-entry-docstring entries) '(nil "Doc." nil "K" "V")))
    (ok (equal (outline-entry-lambda-list (second entries)) "(a &key b)"))
    (ok (every (lambda (e) (equal (outline-entry-package e) "FOO")) (rest entries)))))

(deftest script-header-line-is-not-read
  "A #! first line (Roswell .ros, sbcl --script) is not Lisp: the file reads
clean, the forms after it are found, and the text round-trips."
  (let* ((text (format nil "#!/bin/sh~%#|~%exec ros -Q -- $0 \"$@\"~%|#~%(defun main () 1)~%"))
         (ps (read-source text)))
    (ok (parsed-source-clean-p ps))
    (ok (= 1 (length (parsed-source-forms ps))))
    (ok (string= text (source-string (parsed-source-text ps))))))
