(in-package #:lisp-source-protocol/tests)

(defparameter *pkg-source*
  (lines "(defpackage #:demo.core"
         "  (:use #:cl)"
         "  (:nicknames #:dc)"
         "  (:export #:frob #:widget #:widget-size #:+limit+ #:ghost))"
         "(defpackage #:demo.app"
         "  (:use #:cl #:demo.core)"
         "  (:import-from #:alexandria #:when-let)"
         "  (:shadow #:list)"
         "  (:local-nicknames (#:core #:demo.core)))"
         "(in-package #:demo.core)"
         "(defun frob (x) \"Frob X.\" x)"
         "(defclass widget () ((size :accessor widget-size)))"
         "(unless (boundp '+limit+) (defconstant +limit+ 3))"))

(defparameter *app-source*
  (lines "(in-package #:demo.app)"
         "(defun run () (frob 1) (core:frob 2) (dc:frob 3) (demo.core::frob 4) (when-let ((x 1)) x) (list 1))"))

(defun demo-model ()
  (let ((core (read-source *pkg-source*))
        (app (read-source *app-source*)))
    (values (build-package-model (list core app)) core app)))

(deftest-parametrize resolve-tokens
    ((text package expected-home)
     ("frob" "DEMO.APP" "DEMO.CORE")
     ("core:frob" "DEMO.APP" "DEMO.CORE")
     ("dc:frob" "DEMO.APP" "DEMO.CORE")
     ("when-let" "DEMO.APP" "ALEXANDRIA")
     ("list" "DEMO.APP" "DEMO.APP")
     ("car" "DEMO.APP" "COMMON-LISP")
     ("unknown" "DEMO.APP" "DEMO.APP")
     (":kw" "DEMO.APP" "KEYWORD"))
  (let* ((model (demo-model))
         (token (node-value (first (parsed-source-forms (read-source text))))))
    (ok (equal (resolve-symbol-token model token package) expected-home))))

(deftest package-at-tracks-in-package
  (let* ((ps (read-source (lines "(defun a ())" "(in-package :foo)" "(defun b ())")))
         (forms (parsed-source-forms ps)))
    (ok (equal (package-at ps (node-start (first forms))) "COMMON-LISP-USER"))
    (ok (equal (package-at ps (node-start (third forms))) "FOO"))))

(deftest references-across-files
  (multiple-value-bind (model core app) (demo-model)
    (declare (ignore core))
    (let ((refs (find-references model "demo.core" "frob")))
      ;; the export, the defun name, and four calls in RUN
      (ok (= 6 (length refs)))
      (ok (= 4 (count app refs :key #'car))))
    (ok (= 1 (length (find-definitions model "DC" "FROB"))))
    (ok (null (find-references model "DEMO.APP" "FROB")))))

(defun lint-kinds (source &rest args)
  (mapcar #'diagnostic-kind (apply #'lint (read-source source) args)))

(deftest-parametrize unused-variable
    ((source unused-count)
     ("(defun f (a b) a)" 1)
     ("(defun f (a b) (declare (ignore b)) a)" 0)
     ("(defun f (&key (a 1 a-p)) a)" 1)
     ("(let ((x 1) (y 2)) x)" 1)
     ("(let* ((x 1) (y x)) y)" 0)
     ("(let ((*print-base* 16)) (print 1))" 0)
     ("(defmethod m ((a t) b) 1)" 0)
     ("(flet ((g (u) 1)) (g 2))" 1)
     ("(destructuring-bind (a (b c)) l (list a b))" 1)
     ("(multiple-value-bind (q r) (floor 7 2) q)" 1)
     ("'(let ((x 1)) 2)" 0)
     ("`(let ((x 1)) ,y)" 0))
  (ok (= unused-count (count :unused-variable (lint-kinds source :rules '(:unused-variable))))))

(deftest exports-and-docstrings
  (multiple-value-bind (model core) (demo-model)
    (let ((kinds (mapcar (lambda (d) (cons (diagnostic-kind d) (diagnostic-message d)))
                         (lint core :model model :rules '(:undefined-export :missing-docstring)))))
      (ok (= 1 (count :undefined-export kinds :key #'car)) "only GHOST is undefined")
      (ok (search "GHOST" (cdr (find :undefined-export kinds :key #'car))))
      (ok (= 1 (count :missing-docstring kinds :key #'car)) "WIDGET has no docstring")
      (ok (search "WIDGET" (cdr (find :missing-docstring kinds :key #'car)))))))

(deftest missing-in-package
  (ok (member :missing-in-package (lint-kinds "(defun f () 1)" :rules '(:missing-in-package))))
  (ok (null (lint-kinds (lines "(in-package :x)" "(defun f () 1)") :rules '(:missing-in-package))))
  (ok (null (lint-kinds "(defpackage :x (:use :cl))" :rules '(:missing-in-package)))))

(deftest asd-missing-component
  (let* ((path (asdf:system-relative-pathname "lisp-source-protocol" "probe.asd"))
         (ps (read-source (lines "(defsystem \"probe\""
                                 "  :pathname \"src\""
                                 "  :components ((:file \"package\")"
                                 "               (:file \"no-such-file\")"
                                 "               (:module \"nested\" :components ((:file \"x\")))))")
                          :path (namestring path)))
         (diags (lint ps :rules '(:asd-missing-component))))
    (ok (= 2 (length diags)))
    (ok (every (lambda (d) (eq (diagnostic-kind d) :asd-missing-component)) diags))
    (ok (search "no-such-file.lisp" (diagnostic-message (first diags))))))

(deftest custom-lint-rule
  (defmethod run-lint-rule ((rule (eql :test-no-eval)) parsed context)
    (declare (ignore context))
    (let ((out '()))
      (walk-nodes (lambda (n)
                    (when (equal (node-head-name n) "EVAL")
                      (push (make-lint-diagnostic :test-no-eval :warning n "EVAL call") out)))
                  (parsed-source-forms parsed))
      out))
  (ok (equal (lint-kinds "(progn (eval x))" :rules '(:test-no-eval)) '(:test-no-eval))))
