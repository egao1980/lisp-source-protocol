(in-package #:lisp-source-protocol/tests)

;;; #+ / #- forms are source text. Whatever features the reading image has,
;;; every conditional is a form with a CST of its branch, so edits, outlines
;;; and locators see each branch (lisp-source-protocol#9).

(defparameter *feature-sets*
  '((:sbcl :common-lisp) (:ecl :common-lisp) (:ccl :x86-64))
  "Feature lists to read with: the same source must give the same forms.")

(defun expression-shape (ps)
  "Kind and range of every expression node of PS, depth first."
  (let ((out '()))
    (walk-nodes (lambda (n)
                  (when (expression-node-p n)
                    (push (list (node-kind n) (node-start n) (node-end n)) out)))
                (parsed-source-nodes ps))
    (nreverse out)))

(defun edit-with-features (features source path op &rest args)
  "Apply OP to the node at PATH of SOURCE, everything read with FEATURES;
return the verified new text."
  (let ((*source-features* features))
    (let* ((ps (read-source source))
           (plan (apply op ps (node-at-path ps path) args)))
      (source-string (parsed-source-text (verify-plan plan))))))

;;; Reading

(deftest-parametrize conditionals-are-forms
    ((source features excluded)
     ("#+sbcl (x)" '(:sbcl) nil)
     ("#+sbcl (x)" '(:ecl) t)
     ("#-sbcl (x)" '(:sbcl) t)
     ("#-sbcl (x)" '(:ecl) nil)
     ("#+(or) (x)" '(:sbcl) t)
     ("#-(and) (x)" '(:sbcl) t))
  (let* ((ps (read-source source :features features))
         (forms (parsed-source-forms ps))
         (form (first forms)))
    (ok (parsed-source-clean-p ps))
    (ok (= 1 (length forms)) "one form, included or not")
    (ok (eq :feature-conditional (node-kind form)))
    (ok (equal source (node-text form)))
    (ok (eq excluded (and (feature-excluded-p form) t)))
    (ok (equal '("(x)") (mapcar #'node-text (feature-conditional-forms form))))
    (ok (eq :list (node-kind (first (feature-conditional-forms form))))
        "the branch has a CST, not suppressed input")))

(deftest structure-does-not-depend-on-features
  (let* ((source (lines "#+sbcl (list :plus) #-sbcl (list :minus)"
                        "(progn #+sbcl a #-sbcl b)"
                        "#+(or) (defun old () nil)"))
         (shapes (mapcar (lambda (f) (expression-shape (read-source source :features f)))
                         *feature-sets*)))
    (ok (= 4 (length (parsed-source-forms (read-source source :features '(:sbcl))))))
    (ok (every (lambda (s) (equal s (first shapes))) (rest shapes)))))

(deftest excluded-branches-stay-out-of-diagnostics
  (let ((ps (read-source "#+(or) (a #.(error \"boom\")) #-sbcl (b)" :features '(:sbcl))))
    (ok (parsed-source-clean-p ps))
    (ok (null (parsed-source-diagnostics ps)) "#. in a branch this image skips is not reported")
    (ok (= 2 (length (parsed-source-forms ps))))))

(deftest foreign-syntax-branch-stays-one-form
  "A branch that only reads under its own implementation (here a symbol
with two package markers) stays one opaque form, and edits keep its text."
  (let* ((source (lines "#+ccl (foo:bar:baz)" "" "(a)"))
         (ps (read-source source :features '(:sbcl)))
         (form (first (parsed-source-forms ps))))
    (ok (parsed-source-clean-p ps))
    (ok (= 2 (length (parsed-source-forms ps))))
    (ok (feature-excluded-p form))
    (ok (null (feature-conditional-forms form)))
    (ok (equal (edit-with-features '(:sbcl) "(a)" '(0) #'insert-after "#+ccl (foo:bar:baz) #-ccl (b)")
               (lines "(a)" "" "#+ccl (foo:bar:baz)" "" "#-ccl (b)")))
    (ok (equal (edit-with-features '(:sbcl) source '(0) #'delete-node) "(a)"))))

(deftest broken-form-stops-at-a-column-0-conditional
  "Recovery cuts a form that runs on at the next column-0 #+ / #- too, so a
missing ) does not take the conditional's prefix away from its branch."
  (let ((ps (read-source (lines "(defun a ()" "  (let ((x 1)" "    x))" ""
                                "#-sbcl" "(defun b () 2)" "" "(defun c () 3)")
                         :features '(:sbcl))))
    (ok (= 1 (length (parsed-source-imbalances ps))))
    (ok (equal (rest (texts ps)) (list (lines "#-sbcl" "(defun b () 2)") "(defun c () 3)")))))

(deftest outline-sees-every-branch
  (let ((source (lines "(in-package :p)"
                       "#+sbcl (defun impl () :sbcl)"
                       "#-sbcl (defun impl () :other)"
                       "#+(or) (defun old () nil)")))
    (dolist (features *feature-sets*)
      (let ((entries (outline (read-source source :features features))))
        (ok (equal '("in-package" "defun" "defun" "defun") (mapcar #'outline-entry-kind entries)))
        (ok (equal '("P" "IMPL" "IMPL" "OLD") (mapcar #'outline-entry-name entries)))
        (ok (equal '(nil "#+sbcl" "#-sbcl" "#+(or)")
                   (mapcar (lambda (e) (feature-prefix (outline-entry-node e))) entries)))
        (ok (every (lambda (e) (equal "P" (outline-entry-package e))) (rest entries)))))))

(deftest locators-reach-every-branch
  (let ((source (lines "#+sbcl (defun impl () :sbcl)"
                       "#-sbcl (defun impl () :other)"
                       "(defun f ())")))
    (dolist (features *feature-sets*)
      (let* ((ps (read-source source :features features))
             (second-form (locate ps '(:path 1))))
        (ok (equal "#-sbcl (defun impl () :other)" (node-text second-form)))
        (ok (equal ":other" (node-text (locate ps '(:path 1 1 3)))))
        (ok (equal ":other" (node-text (locate ps '(:line 2 :column 23)))))
        (ok (eq second-form (locate ps (list :hash (node-hash second-form)))))
        (ok (equal '("(defun impl () :sbcl)" "(defun impl () :other)")
                   (mapcar #'node-text (find-definition-nodes ps "defun" "impl"))))
        (ok (signals (locate ps '(:definition "defun" "impl")) 'ambiguous-locator))
        (ok (equal "(defun f ())" (node-text (locate ps '(:path 2)))))))))

(deftest references-reach-every-branch
  (dolist (features *feature-sets*)
    (let* ((*source-features* features)
           (ps (read-source (lines "(in-package :cl-user)" "#+sbcl (frob 1)" "#-sbcl (frob 2)")))
           (model (build-package-model (list ps)))
           (plans (rename-symbol model "COMMON-LISP-USER" "FROB" "twiddle")))
      (ok (= 2 (length (find-references model "COMMON-LISP-USER" "FROB"))))
      (ok (equal (source-string (parsed-source-text (verify-plan (first plans))))
                 (lines "(in-package :cl-user)" "#+sbcl (twiddle 1)" "#-sbcl (twiddle 2)"))
          (format nil "rename reaches both branches, features ~S" features)))))

;;; Edit input: every branch is written, whatever the features

(defparameter *save-hook-branches*
  (lines "#+sbcl (pushnew '%reset-boundary-random-state sb-ext:*save-hooks*)"
         "#-sbcl (uiop:register-image-dump-hook '%reset-boundary-random-state)"))

(deftest-parametrize edit-input-keeps-every-branch
    ((source path op args expected)
     ((lines "(a)" "" "(b)") '(0) #'insert-after '("#+sbcl (list :plus) #-sbcl (list :minus)")
      (lines "(a)" "" "#+sbcl (list :plus)" "" "#-sbcl (list :minus)" "" "(b)"))
     ((lines "(a)" "" "(b)") '(1) #'insert-before '("#+sbcl (list :plus) #-sbcl (list :minus)")
      (lines "(a)" "" "#+sbcl (list :plus)" "" "#-sbcl (list :minus)" "" "(b)"))
     ("(defun f ())" '(0) #'insert-after (list *save-hook-branches*)
      (lines "(defun f ())" ""
             "#+sbcl (pushnew '%reset-boundary-random-state sb-ext:*save-hooks*)" ""
             "#-sbcl (uiop:register-image-dump-hook '%reset-boundary-random-state)"))
     ("(a)" '(0) #'insert-after '("#+(or) (old) #-(and) (older)")
      (lines "(a)" "" "#+(or) (old)" "" "#-(and) (older)"))
     ("(progn (a))" '(0 1) #'insert-after '("#-sbcl (b) #+sbcl (c)")
      "(progn (a) #-sbcl (b) #+sbcl (c))")
     ("(a (b) c)" '(0 1) #'replace-node '("#-sbcl (list :minus)")
      "(a #-sbcl (list :minus) c)")
     ((lines "(a)" "" "(b)") '(1) #'replace-node '("#+(or) (x)")
      (lines "(a)" "" "#+(or) (x)"))
     ((lines "(a)" "" "(b)") '(1) #'replace-node '("#-(and) (x)")
      (lines "(a)" "" "#-(and) (x)")))
  (dolist (features *feature-sets*)
    (ok (equal (apply #'edit-with-features features source path op args) expected)
        (format nil "features ~S" features))))

(deftest replace-counts-every-branch
  (dolist (features *feature-sets*)
    (let* ((*source-features* features)
           (ps (read-source "(a (b) c)")))
      (ok (signals (replace-node ps (node-at-path ps '(0 1)) "#+sbcl (x) #-sbcl (y)")
                   'edit-input-unreadable)
          "two forms are not one replacement form"))))

;;; Edits on conditionals already in a file

(deftest-parametrize edits-on-existing-branches
    ((source path op args expected)
     ((lines "(a)" "" "#-sbcl (b)" "" "(c)") '(1) #'delete-node '()
      (lines "(a)" "" "(c)"))
     ;; the branch's own form, as a definition locator finds it: the prefix goes too
     ((lines "(a)" "" "#-sbcl (defun b ())" "" "(c)") '(1 1) #'delete-node '()
      (lines "(a)" "" "(c)"))
     ((lines "(a)" "" "#+(or) (b)" "") '(1) #'delete-node '()
      (lines "(a)" ""))
     ((lines "#+sbcl (a)" "" "#-sbcl (b)") '(0) #'transpose-nodes '()
      (lines "#-sbcl (b)" "" "#+sbcl (a)"))
     ((lines "#+sbcl (a)" "" "#-sbcl (b)") '(0 1) #'transpose-nodes '()
      (lines "#-sbcl (b)" "" "#+sbcl (a)"))
     ("#-sbcl (defun f ())" '(0 1) #'insert-after '("(g)")
      (lines "#-sbcl (defun f ())" "" "(g)"))
     ("#-sbcl (defun f ())" '(0 1) #'wrap-node '("progn")
      "#-sbcl (progn (defun f ()))")
     ("#-sbcl (defun f () 1)" '(0 1 3) #'replace-node '("2")
      "#-sbcl (defun f () 2)"))
  (dolist (features *feature-sets*)
    (ok (equal (apply #'edit-with-features features source path op args) expected)
        (format nil "features ~S" features))))

(deftest move-carries-the-branch
  (dolist (features *feature-sets*)
    (let* ((*source-features* features)
           (ps (read-source (lines "#-sbcl (defun a ())" "" "(b)")))
           (plans (move-node ps (node-at-path ps '(0 1)) ps (node-at-path ps '(1)))))
      (ok (= 1 (length plans)))
      (ok (equal (source-string (parsed-source-text (verify-plan (first plans))))
                 (lines "(b)" "" "#-sbcl (defun a ())"))
          (format nil "features ~S" features)))))
