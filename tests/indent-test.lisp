(in-package #:lisp-source-protocol/tests)

(deftest-parametrize indent-rules
    ((input expected)
     ;; distinguished arguments and body
     ((lines "(defun foo (x)" "(let ((a 1)" "(b 2))" "(when x" "(print a))" "a))")
      (lines "(defun foo (x)" "  (let ((a 1)" "        (b 2))" "    (when x" "      (print a))" "    a))"))
     ;; function call: align with the first argument, or one past the paren
     ((lines "(list a" "b)") (lines "(list a" "      b)"))
     ((lines "(list" "a b)") (lines "(list" " a b)"))
     ;; keyword/value pairs after a leading argument
     ((lines "(make-instance 'foo :a 1" ":b 2)") (lines "(make-instance 'foo :a 1" "                    :b 2)"))
     ;; data
     ((lines "'(1 2" "3)") (lines "'(1 2" "  3)"))
     ((lines "(:export #:a" "#:b)") (lines "(:export #:a" " #:b)"))
     ((lines "((a b)" "(c d))") (lines "((a b)" " (c d))"))
     ;; local definitions and clauses
     ((lines "(flet ((f (x)" "(1+ x)))" "(f 1))") (lines "(flet ((f (x)" "         (1+ x)))" "  (f 1))"))
     ((lines "(handler-case (foo)" "(error (e)" "(print e)))")
      (lines "(handler-case (foo)" "  (error (e)" "    (print e)))"))
     ((lines "(handler-case (foo)" "((or a b) ()" "nil))") (lines "(handler-case (foo)" "  ((or a b) ()" "    nil))"))
     ;; unknown def / with- operators, package prefixes
     ((lines "(define-thing x y" "body)") (lines "(define-thing x y" "  body)"))
     ((lines "(bt2:with-lock-held (l)" "body)") (lines "(bt2:with-lock-held (l)" "  body)"))
     ;; strings, block comments and characters are not disturbed
     ((lines "(defun x ()" "\"doc" "   kept\"" "(f #\\( 1" "2))")
      (lines "(defun x ()" "  \"doc" "   kept\"" "  (f #\\( 1" "     2))"))
     ((lines "(progn" "#| a" "     b |#" "(f))") (lines "(progn" "  #| a" "     b |#" "  (f))"))
     ;; LOOP keeps its clause layout
     ((lines "(loop for x in y" "      when x" "        collect x)") (lines "(loop for x in y" "      when x" "        collect x)")))
  (ok (equal (indent-code input) expected)))

(deftest indent-at-a-column
  (ok (equal (indent-code (lines "(when x" "(f))") :column 4) (lines "(when x" "      (f))")))
  ;; LOOP clauses move with the form
  (ok (equal (indent-code (lines "(loop for x in y" "      collect x)") :column 2 :input-column 0)
             (lines "(loop for x in y" "        collect x)"))))

(deftest set-indentation-specs
  (unwind-protect
       (progn
         (set-indentation "my-macro" '(4 4 &body))
         (ok (eql 2 (operator-indentation "pkg:my-macro")))
         (ok (equal (indent-code (lines "(my-macro a b" "c)")) (lines "(my-macro a b" "  c)")))
         (set-indentation "my-macro" nil)
         (ok (null (operator-indentation "my-macro"))))
    (reset-indentation)))

(deftest learn-indentation-from-defmacro
  (unwind-protect
       (let ((ps (read-source (lines "(defmacro define-widget (name (&key size) &body body) nil)"
                                     "(defmacro plain (a b) nil)"))))
         (ok (= 1 (learn-indentation (list ps))))
         (ok (eql 2 (operator-indentation "define-widget")))
         (ok (equal (indent-code (lines "(define-widget w (:size 1)" "(draw))"))
                    (lines "(define-widget w (:size 1)" "  (draw))"))))
    (reset-indentation)))

(deftest edits-reindent-inserted-code
  (ok (equal (run-op (lines "(defun f ()" "  (old))") '(0 3) #'replace-node
                     (lines "(when x" "(foo)" "(bar))"))
             (lines "(defun f ()" "  (when x" "    (foo)" "    (bar)))")))
  (ok (equal (run-op (lines "(defun f ()" "  (foo a" "       b))") '(0 3) #'wrap-node "when x")
             (lines "(defun f ()" "  (when x (foo a" "               b)))")))
  (ok (equal (let ((*reindent* nil))
               (run-op (lines "(defun f ()" "  (old))") '(0 3) #'replace-node (lines "(a" " b)")))
             (lines "(defun f ()" "  (a" "   b))"))))

(defun move-text (source from to where)
  (let* ((ps (read-source source))
         (plans (move-node ps (node-at-path ps from) ps (node-at-path ps to) :where where)))
    (ok (= 1 (length plans)))
    (source-string (parsed-source-text (verify-plan (first plans))))))

(deftest-parametrize move-within-a-file
    ((source from to where expected)
     ("(a b c d)" '(0 1) '(0 3) :after "(a c d b)")
     ("(a b c d)" '(0 3) '(0 1) :before "(a d b c)")
     ("(a b c d)" '(0 1) '(0 2) :before "(a b c d)")
     ("(a (b c) d)" '(0 2) '(0 1 1) :after "(a (b c d))")
     ((lines "(defun a ()" "  1)" "" "(defun b ()" "  2)")
      '(0) '(1) :after
      (lines "(defun b ()" "  2)" "" "(defun a ()" "  1)"))
     ((lines "(let ()" "  (when x" "    (f)))" "(g)")
      '(0 2) '(1) :after
      (lines "(let ())" "(g)" "" "(when x" "  (f))")))
  (ok (equal (move-text source from to where) expected)))

(deftest move-between-files
  (let* ((a (read-source (lines "(in-package :x)" "" "(defun f ()" "  (g))" "" "(defun h () 1)") :path "a.lisp"))
         (b (read-source (lines "(in-package :x)" "" "(defun z () 0)") :path "b.lisp"))
         (plans (move-node a (node-at-path a '(1)) b (node-at-path b '(1)))))
    (ok (= 2 (length plans)))
    (ok (equal (source-string (parsed-source-text (verify-plan (first plans))))
               (lines "(in-package :x)" "" "(defun h () 1)")))
    (ok (equal (source-string (parsed-source-text (verify-plan (second plans))))
               (lines "(in-package :x)" "" "(defun z () 0)" "" "(defun f ()" "  (g))")))))

(deftest move-refuses-nonsense
  (let ((ps (read-source "(a (b c) d)")))
    (ok (signals (move-node ps (node-at-path ps '(0 1)) ps (node-at-path ps '(0 1 1)))
                 'edit-not-applicable))
    (ok (signals (move-node ps (node-at-path ps '(0 1)) ps (node-at-path ps '(0 1)))
                 'edit-not-applicable))))
