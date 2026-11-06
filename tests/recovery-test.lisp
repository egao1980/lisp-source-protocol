(in-package #:lisp-source-protocol/tests)

(defun only-imbalance (ps)
  (let ((imbalances (parsed-source-imbalances ps)))
    (ok (= 1 (length imbalances)))
    (first imbalances)))

(defun fix-line (ps imbalance)
  (offset-line-column (parsed-source-text ps) (imbalance-fix-position imbalance)))

(deftest missing-close-stays-local
  (let* ((ps (read-source (lines "(defun a (x)"
                                 "  (let ((y 1))"
                                 "    (+ x y))"
                                 ""
                                 "(defun b () 2)"
                                 "(defun c () 3)")))
         (i (only-imbalance ps)))
    (ok (equal (rest (texts ps)) '("(defun b () 2)" "(defun c () 3)")))
    (ok (eq (imbalance-kind i) :missing-close))
    (ok (= (imbalance-count i) 1))
    (ok (= (fix-line ps i) 3) "insert after (+ x y))")
    (ok (node-recovered-p (first (parsed-source-forms ps))))
    (ng (node-recovered-p (second (parsed-source-forms ps))))))

(deftest missing-close-uses-indentation
  ;; The let never closes; the dedent to column 2 says it should close on line 3.
  (let* ((ps (read-source (lines "(defun a (x)"
                                 "  (let ((y 1))"
                                 "    (+ x y)"
                                 "  (print x))"
                                 "(defun b () 2)")))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :missing-close))
    (ok (eq (imbalance-confidence i) :high))
    (ok (= (fix-line ps i) 3))
    (ok (equal (second (texts ps)) "(defun b () 2)"))))

(deftest extra-close-points-at-early-closer
  (let* ((source (lines "(defun a (x)"
                        "  (let ((y 1)))"
                        "    (+ x y)))"
                        "(defun b () 2)"))
         (ps (read-source source))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :extra-close))
    ;; "(let ((y 1)))": the third ) after (y 1 closes the let too early.
    (ok (= (imbalance-fix-position i) (+ (search "(y 1)" source) 6)))
    (ok (= (fix-line ps i) 2))
    (ok (equal (car (last (texts ps))) "(defun b () 2)"))))

(deftest stray-close-at-top-level
  (let* ((ps (read-source "(a) ) (b)"))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :extra-close))
    (ok (= (imbalance-open-position i) 4))
    (ok (equal (texts ps) '("(a)" "(b)")))))

(deftest unterminated-string-stays-local
  (let* ((ps (read-source (lines "(defun a () \"unterminated" "  1)" "(defun b () 2)")))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :unterminated-string))
    (ok (= (fix-line ps i) 1))
    (ok (equal (second (texts ps)) "(defun b () 2)"))))

(deftest unterminated-block-comment-stays-local
  (let* ((ps (read-source (lines "#| open" "(defun b () 2)")))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :unterminated-block-comment))
    (ok (equal (texts ps) '("(defun b () 2)")))))

(deftest broken-at-end-of-input
  (let* ((ps (read-source "(a) (b (c)"))
         (i (only-imbalance ps)))
    (ok (eq (imbalance-kind i) :missing-close))
    (ok (equal (first (texts ps)) "(a)"))
    (ok (null (first-uncovered ps)))))

(deftest clean-source-has-no-imbalance
  ;; Column-0 parens inside a well-formed literal must not trigger a resync.
  (let ((ps (read-source (lines "(defparameter *table* '("
                                "(a 1)"
                                "(b 2)))"
                                "(defun f () *table*)"))))
    (ok (parsed-source-clean-p ps))
    (ok (= 2 (length (parsed-source-forms ps))))))
