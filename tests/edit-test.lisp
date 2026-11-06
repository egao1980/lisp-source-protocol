(in-package #:lisp-source-protocol/tests)

(defun run-op (source path op &rest args)
  "Apply OP to the node at PATH in SOURCE; return the verified new text."
  (let* ((ps (read-source source))
         (plan (apply op ps (node-at-path ps path) args)))
    (source-string (parsed-source-text (verify-plan plan)))))

(deftest-parametrize structural-operations
    ((source path op args expected)
     ("(a (b c) d)" '(0 1) #'replace-node '("(x y)") "(a (x y) d)")
     ("(a b c)" '(0 1) #'insert-after '("z") "(a b z c)")
     ("(a b c)" '(0 1) #'insert-before '("z q") "(a z q b c)")
     ((lines "(a)" "(b)") '(0) #'insert-after '("(z)") (lines "(a)" "" "(z)" "(b)"))
     ((lines "(let ((x 1))" "  (foo)" "  (bar))") '(0 2) #'insert-before '("(baz)")
      (lines "(let ((x 1))" "  (baz)" "  (foo)" "  (bar))"))
     ("(a b c)" '(0 1) #'delete-node '() "(a c)")
     ("(a b c)" '(0 2) #'delete-node '() "(a b)")
     ((lines "(foo a" "     b)") '(0 2) #'delete-node '() "(foo a)")
     ((lines "(a)" "" "(b)" "") '(0) #'delete-node '() (lines "(b)" ""))
     ("(a b)" '(0 1) #'wrap-node '("when x") "(a (when x b))")
     ("(a b)" '(0 1) #'wrap-node '(nil) "(a (b))")
     ("(a (b c) d)" '(0 1) #'splice-node '() "(a b c d)")
     ("(a (b c) d)" '(0 1 1) #'raise-node '() "(a c d)")
     ("(a (b) c d)" '(0 1) #'slurp-forward '() "(a (b c) d)")
     ("(a b (c) d)" '(0 2) #'slurp-backward '() "(a (b c) d)")
     ("(a (b c d))" '(0 1) #'barf-forward '() "(a (b c) d)")
     ("(a (b c d))" '(0 1) #'barf-backward '() "(a b (c d))")
     ("(a b c)" '(0 1) #'transpose-nodes '() "(a c b)")
     ((lines "(a)" "(b)") '(0) #'slurp-forward '() (lines "(a" "(b))"))
     ("(a) (b c)" '(1) #'barf-forward '() "(a) (b) c")
     ((lines "(defun f ()" "  (old))") '(0 3) #'replace-node
      (list (lines "(let ((x 1))" "  x)"))
      (lines "(defun f ()" "  (let ((x 1))" "    x))")))
  (ok (equal (apply #'run-op source path op args) expected)))

(deftest unreadable-input-is-rejected
  (let* ((ps (read-source "(a b)"))
         (node (node-at-path ps '(0 1))))
    (ok (signals (replace-node ps node "(x") 'edit-input-unreadable))
    (ok (signals (replace-node ps node "x y") 'edit-input-unreadable) "exactly one form")
    (ok (signals (insert-after ps node "\"open") 'edit-input-unreadable))
    (ok (signals (wrap-node ps node "when (x") 'edit-input-unreadable))))

(deftest operation-preconditions
  (let ((ps (read-source "(a b) (c)")))
    (ok (signals (splice-node ps (node-at-path ps '(0 1))) 'edit-not-applicable))
    (ok (signals (slurp-forward ps (node-at-path ps '(1))) 'edit-not-applicable) "no next sibling")
    (ok (signals (raise-node ps (node-at-path ps '(0))) 'edit-not-applicable))
    (ok (signals (barf-forward ps (node-at-path ps '(1 0))) 'edit-not-applicable) "c is a symbol")))

(deftest verify-refuses-unbalancing-plans
  (let* ((source "(a (b c) d)")
         (ps (read-source source))
         (plan (make-instance 'edit-plan :parsed ps :operation :manual
                                         :edits (list (make-text-edit 7 8 "")))))
    (ok (signals (verify-plan plan) 'edit-would-unbalance))
    ;; a plan that keeps balance but lies about the form delta
    (let ((liar (make-instance 'edit-plan :parsed ps :operation :manual :form-delta 0
                                          :edits (list (make-text-edit 11 11 " (e)")))))
      (ok (signals (verify-plan liar) 'edit-would-unbalance)))))

(deftest recovered-nodes-are-protected
  (let* ((ps (read-source (lines "(defun a (x)" "  (let ((y 1))" "    (+ x y))" "(defun b () 2)")))
         (broken (first (parsed-source-forms ps))))
    (ok (signals (delete-node ps (first (node-forms broken))) 'edit-on-recovered-node))
    ;; other forms stay editable
    (ok (verify-plan (replace-node ps (node-at-path ps '(1 3)) "3")))))

(deftest repair-parens-fixes-imbalances
  (dolist (source (list (lines "(defun a (x)" "  (let ((y 1))" "    (+ x y))" "" "(defun b () 2)")
                        (lines "(defun a (x)" "  (let ((y 1)))" "    (+ x y)))" "(defun b () 2)")))
    (let* ((ps (read-source source))
           (fixed (verify-plan (repair-parens ps))))
      (ok (parsed-source-clean-p fixed))
      (ok (= 2 (length (parsed-source-forms fixed))))))
  (ok (signals (repair-parens (read-source "(a)")) 'edit-not-applicable)))

(deftest stale-hash-is-refused
  (let* ((ps (read-source "(a b)"))
         (plan (replace-node ps (node-at-path ps '(0 1)) "c")))
    (ok (signals (commit-plan plan :dry-run t :expected-hash "0000") 'stale-source))
    (ok (handler-bind ((stale-source #'continue))
          (commit-plan plan :dry-run t :expected-hash "0000")))
    (ok (commit-plan plan :dry-run t :expected-hash (parsed-source-hash ps)))))

(deftest rename-across-files
  (multiple-value-bind (model core app) (demo-model)
    (let* ((plans (rename-symbol model "DEMO.CORE" "FROB" "twiddle"))
           (texts (mapcar (lambda (p) (source-string (parsed-source-text (verify-plan p)))) plans)))
      (ok (= 2 (length plans)))
      (ok (search "#:twiddle" (first texts)) "defpackage export renamed")
      (ok (search "(defun twiddle (x)" (first texts)))
      (ok (search "(twiddle 1) (core:twiddle 2) (dc:twiddle 3) (demo.core::twiddle 4)" (second texts)))
      (ok (eq (edit-plan-parsed (first plans)) core))
      (ok (eq (edit-plan-parsed (second plans)) app)))))

(deftest add-export-keeps-style
  (let* ((ps (read-source (lines "(defpackage #:p"
                                 "  (:use #:cl)"
                                 "  (:export #:a"
                                 "           #:b))")))
         (text (source-string (parsed-source-text (verify-plan (add-export ps "P" "NEW-THING"))))))
    (ok (search (lines "           #:b" "           #:new-thing))") text)))
  (let* ((ps (read-source "(defpackage :q (:use :cl))"))
         (text (source-string (parsed-source-text (verify-plan (add-export ps "Q" "x"))))))
    (ok (search "(:export #:x)" text)))
  (let ((ps (read-source "(defpackage :q (:export :x))")))
    (ok (null (edit-plan-edits (add-export ps "Q" "X"))) "already exported")))

(deftest commit-writes-atomically
  (let ((path (merge-pathnames (format nil "lsp-edit-~D.lisp" (random 1000000 (make-random-state t)))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (alexandria:write-string-into-file (lines "(in-package :cl-user)" "(defun f () 1)" "")
                                              path :if-exists :supersede)
           (let* ((ps (read-source-file path))
                  (plan (replace-node ps (node-at-path ps '(1 3)) "2"))
                  (dry (commit-plan plan :dry-run t)))
             (ok (not (edit-result-written-p dry)))
             (ok (search "(defun f () 1)" (uiop:read-file-string path)))
             (ok (search "-(defun f () 1)" (edit-result-diff dry)))
             (ok (search "+(defun f () 2)" (edit-result-diff dry)))
             (let ((done (commit-plan plan :expected-hash (parsed-source-hash ps))))
               (ok (edit-result-written-p done))
               (ok (search "(defun f () 2)" (uiop:read-file-string path))))
             ;; the file changed under us: the old plan is now stale
             (ok (signals (commit-plan plan) 'stale-source))))
      (uiop:delete-file-if-exists path))))

(deftest unified-diff-hunks
  (let ((diff (unified-diff (lines "a" "b" "c" "d" "e" "f" "g" "h" "i")
                            (lines "a" "B" "c" "d" "e" "f" "g" "H" "i"))))
    (ok (= 2 (count-matches "@@ -" diff)))
    (ok (search "-b" diff))
    (ok (search "+H" diff)))
  (ok (string= "" (unified-diff "x" "x"))))

(defun count-matches (needle haystack)
  (loop for start = 0 then (1+ pos)
        for pos = (search needle haystack :start2 start)
        while pos count t))

(deftest-parametrize prefixes-and-dots-are-not-sibling-slots
    ((source path op)
     ("(a #+sbcl b c)" '(0 1 0) #'delete-node)
     ("(a 'b c)" '(0 1 0) #'insert-after)
     ("(a . b)" '(0 1) #'wrap-node)
     ("(a . b)" '(0 0) #'delete-node)
     ("(x (a . b))" '(0 1) #'barf-forward)
     ("(a #+sbcl (b) c)" '(0 1 0) #'raise-node))
  (let ((ps (read-source source :features '(:sbcl))))
    (ok (signals (let ((node (node-at-path ps path)))
                   (if (member op (list #'insert-after))
                       (funcall op ps node "z")
                       (if (eq op #'wrap-node) (funcall op ps node "w") (funcall op ps node))))
                 'edit-not-applicable))))
