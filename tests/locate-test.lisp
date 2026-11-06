(in-package #:lisp-source-protocol/tests)

(defparameter *locate-source*
  (lines "(in-package :foo)"
         "(defun f (x) x)"
         "(defmethod print-object ((o my-class) s) o)"
         "(defmethod print-object ((o other) s) o)"
         "(progn (defvar *v* 1))"))

(deftest-parametrize locate-definitions
    ((locator expected-start)
     ('(:definition "defun" "f") "(defun f")
     ('(:definition "defmethod" "print-object" "(my-class t)") "(defmethod print-object ((o my-class)")
     ('(:definition "defmethod" "print-object" "(other  T)") "(defmethod print-object ((o other)")
     ('(:definition "DEFVAR" "*v*") "(defvar *v*")
     ('(:path 1 2) "(x)")
     ('(:line 2 :column 0) "(defun f"))
  (let ((node (locate (read-source *locate-source*) locator)))
    (ok (eql 0 (search expected-start (node-text node))))))

(deftest locate-by-offset-is-innermost
  (let* ((ps (read-source "(a (b c))"))
         (node (locate ps '(:offset 6))))
    (ok (string= (node-text node) "c"))))

(deftest locate-by-hash
  (let* ((ps (read-source *locate-source*))
         (form (second (parsed-source-forms ps))))
    (ok (eq form (locate ps (list :hash (node-hash form)))))))

(deftest locate-not-found
  (ok (signals (locate (read-source *locate-source*) '(:definition "defun" "nope"))
               'locator-not-found))
  (ok (signals (locate (read-source *locate-source*) '(:path 9))
               'locator-not-found)))

(deftest ambiguous-locator-offers-use-value
  (let ((ps (read-source *locate-source*)))
    (ok (signals (locate ps '(:definition "defmethod" "print-object")) 'ambiguous-locator))
    (let ((picked (handler-bind ((ambiguous-locator
                                   (lambda (c)
                                     (use-value (second (ambiguous-locator-candidates c)) c))))
                    (locate ps '(:definition "defmethod" "print-object")))))
      (ok (search "other" (node-text picked))))))
