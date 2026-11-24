(in-package #:lisp-source-protocol/tests)

;;; Layout of inserted and replaced code.
;;;   - Comments in the input reach the file where they were written
;;;     (lisp-source-protocol#11, lisp-mcp#28), and a comment that ends the
;;;     anchor's line stays there (#14 item 3).
;;;   - Re-indentation sees the forms around the edit: a replaced LABELS
;;;     binding or HANDLER-CASE clause indents like one (#14 item 2), and COND
;;;     clauses go 2 in, as in SLIME (#14 item 1).
;;;   - A form inserted, moved or slurped first or last into a list whose
;;;     elements sit on their own lines gets its own line (#15).

(defun check-edit (source path op args expected)
  "Apply OP at PATH of SOURCE; the result must be EXPECTED. A failure shows both."
  (let ((actual (apply #'run-op source path op args)))
    (ok (equal actual expected)
        (if (equal actual expected)
            (format nil "edit at ~S" path)
            (format nil "edit at ~S gave~%~A~%expected~%~A" path actual expected)))))

(deftest-parametrize inserted-comments-are-kept
    ((source path op args expected)
     ;; lisp-source-protocol#11
     ("(a)" '(0) #'insert-after (list (lines ";; about b" "(b) ; trailing" "" ";; about c" "(c)"))
      (lines "(a)" "" ";; about b" "(b) ; trailing" "" ";; about c" "(c)"))
     ;; lisp-mcp#28
     ("(defun probe-a () 1)" '(0) #'insert-after
      (list (lines ";;; Section comment before the first inserted form." ""
                   "(defun probe-b ()" "  ;; inner comment" "  2)" ""
                   ";;; Comment between forms." "" "(defun probe-c () 3)"))
      (lines "(defun probe-a () 1)" "" ";;; Section comment before the first inserted form." ""
             "(defun probe-b ()" "  ;; inner comment" "  2)" ""
             ";;; Comment between forms." "" "(defun probe-c () 3)"))
     ;; before a top-level form: above the comment lines that belong to it
     ((lines "(a)" "" ";; about b" "(b)") '(1) #'insert-before (list (lines ";; about z" "(z) ; zed"))
      (lines "(a)" "" ";; about z" "(z) ; zed" "" ";; about b" "(b)"))
     ;; after a top-level form: below the comment that ends its line
     ((lines "(a) ; about a" "" "(c)") '(0) #'insert-after '("(b)")
      (lines "(a) ; about a" "" "(b)" "" "(c)"))
     ;; block comments, alone and between forms on a line
     ("(a)" '(0) #'insert-after (list (lines "#| block" "   comment |#" "(b) #| inline |# (c)"))
      (lines "(a)" "" "#| block" "   comment |#" "(b) #| inline |# (c)"))
     ;; inside a form
     ((lines "(defun f ()" "  (a)" "  (c))") '(0 3) #'insert-after (list (lines ";; step b" "(b) ; trailing"))
      (lines "(defun f ()" "  (a)" "  ;; step b" "  (b) ; trailing" "  (c))"))
     ;; a trailing comment of the last inserted form goes after the closers
     ((lines "(defun f ()" "  (a))") '(0 3) #'insert-after '("(b) ; why")
      (lines "(defun f ()" "  (a)" "  (b)) ; why"))
     ;; ...and pushes a sibling on the same line to the next line
     ("(f a b c)" '(0 2) #'insert-after '("z ; note") (lines "(f a b z ; note" "   c)"))
     ("(f a b c)" '(0 2) #'insert-before '("z ; note") (lines "(f a z ; note" "   b c)"))
     ;; replace keeps comments around the form, and puts them in place
     ((lines "(defun f ()" "  (old))") '(0 3) #'replace-node (list (lines ";; new" "(new) ; trailing"))
      (lines "(defun f ()" "  ;; new" "  (new)) ; trailing"))
     ("(f a (old) c)" '(0 2) #'replace-node '("(new) ; note") (lines "(f a (new) ; note" "   c)"))
     ((lines "(defun f ()" "  (old))") '(0 3) #'replace-node
      (list (lines "(progn" ";; inside" "(a) ; t" "#| block |#" "(b))"))
      (lines "(defun f ()" "  (progn" "    ;; inside" "    (a) ; t" "    #| block |#" "    (b)))")))
  (check-edit source path op args expected))

(defparameter *let-star*
  (lines "(let* ((a 1)"
         "       (http-connect-p nil) ; HTTPS via HTTP proxy → CONNECT then TLS"
         "       (b 2))"
         "  body)"))

(deftest-parametrize anchor-comments-stay-on-their-line
    ((source path op args expected)
     ;; #14 item 3: new bindings with comments after one with a comment
     (*let-star* '(0 1 1) #'insert-after
      (list (lines "(proxy-tls-p nil) ; TLS to the proxy" "(proxy-host nil)  ; host"))
      (lines "(let* ((a 1)"
             "       (http-connect-p nil) ; HTTPS via HTTP proxy → CONNECT then TLS"
             "       (proxy-tls-p nil) ; TLS to the proxy"
             "       (proxy-host nil)  ; host"
             "       (b 2))"
             "  body)"))
     ;; the anchor is last: its comment, after the closers, stays on its line
     ((lines "(let* ((a 1)" "       (http-connect-p nil)) ; HTTPS via HTTP proxy" "  body)")
      '(0 1 1) #'insert-after '("(proxy-host nil) ; host")
      (lines "(let* ((a 1)" "       (http-connect-p nil) ; HTTPS via HTTP proxy"
             "       (proxy-host nil)) ; host" "  body)"))
     ;; inserting before a form goes above the comment lines over it
     ((lines "(let ((a 1)" "      ;; why b" "      (b 2))" "  body)") '(0 1 1) #'insert-before '("(z 0) ; zero")
      (lines "(let ((a 1)" "      (z 0) ; zero" "      ;; why b" "      (b 2))" "  body)")))
  (check-edit source path op args expected))

(deftest-parametrize cond-clauses-indent-by-two
    ((input expected)
     ((lines "(cond" "((plusp ret) (values :ok ret))" "(t nil))")
      (lines "(cond" "  ((plusp ret) (values :ok ret))" "  (t nil))"))
     ((lines "(cond ((a) b)" "((c) d))") (lines "(cond ((a) b)" "      ((c) d))"))
     ((lines "(cond ((and a" "b)" "(foo))" "(t" "(bar)))")
      (lines "(cond ((and a" "            b)" "       (foo))" "      (t" "       (bar)))"))
     ((lines "(cl:cond" "(x 1))") (lines "(cl:cond" "  (x 1))"))
     ((lines "(defun f (x)" "(cond" "((plusp x) 1)" "(t" "0)))")
      (lines "(defun f (x)" "  (cond" "    ((plusp x) 1)" "    (t" "     0)))")))
  (ok (equal (indent-code input) expected) (format nil "~A" (indent-code input))))

(deftest replaced-cond-keeps-its-layout
  "#14 item 1: replacing a defun with a COND in it changed only whitespace."
  (check-edit (lines "(defun %ssl-step (ret)" "  (old))") '(0 3) #'replace-node
              (list (lines "(cond" "  ((plusp ret) (values :ok ret))" "  (t (values :error ret)))"))
              (lines "(defun %ssl-step (ret)" "  (cond" "    ((plusp ret) (values :ok ret))"
                     "    (t (values :error ret))))")))

(deftest rest-specs
  (unwind-protect
       (progn
         (set-indentation "my-cond" '(:rest 2))
         (ok (equal (indent-code (lines "(my-cond" "(a b))")) (lines "(my-cond" "  (a b))")))
         (ok (equal (indent-code (lines "(my-cond (a b)" "(c d))")) (lines "(my-cond (a b)" "         (c d))"))))
    (reset-indentation)))

(deftest-parametrize binding-values-indent-by-two
    ((input expected)
     ;; SLIME: a binding's value on its own line goes 2 in (let, handler-bind)
     ((lines "(let ((x" "(foo))" "(y 2))" "x)") (lines "(let ((x" "        (foo))" "      (y 2))" "  x)"))
     ((lines "(let* ((first-arg (f 1))" "(keyword-arg" "(and a b)))" "body)")
      (lines "(let* ((first-arg (f 1))" "       (keyword-arg" "         (and a b)))" "  body)"))
     ((lines "(handler-bind ((error" "(lambda (c) c)))" "(foo))")
      (lines "(handler-bind ((error" "                 (lambda (c) c)))" "  (foo))"))
     ;; ...but aligns with a value that starts on the binding's line
     ((lines "(let ((sock-type #+sbcl" "(cond" "(a b))))" "x)")
      (lines "(let ((sock-type #+sbcl" "                 (cond" "                   (a b))))" "  x)"))
     ;; the binding list itself, and the body
     ((lines "(let" "((a 1))" "a)") (lines "(let" "    ((a 1))" "  a)"))
     ;; an unquoted form in the bindings' place is code, not bindings
     ((lines "(let ,(list 'a" "'b)" "x)") (lines "(let ,(list 'a" "            'b)" "  x)"))
     ((lines "(flet ,(list 'a" "'b)" "x)") (lines "(flet ,(list 'a" "             'b)" "  x)")))
  (ok (equal (indent-code input) expected) (format nil "~A" (indent-code input))))

(deftest move-keeps-string-lines
  "Moving a form keeps the lines of its strings: their text is the string."
  (let* ((source (lines "(defun a ()" "  (let ((x 1))" "    (f x \"one" "       two\")))" "" "(defun b ()" "  (g))"))
         (ps (read-source source))
         (plans (move-node ps (node-at-path ps '(0 3 2)) ps (node-at-path ps '(1 3))))
         (actual (source-string (parsed-source-text (verify-plan (first plans))))))
    (ok (search (lines "  (g)" "  (f x \"one" "       two\"))") actual) actual)))

(defparameter *outer*
  (lines "(defun outer ()"
         "  (let ((a 1))"
         "    (labels ((touch ()"
         "               \"Doc.\""
         "               (setf a 2))"
         "             (other (x)"
         "               (list x"
         "                     a)))"
         "      (touch)"
         "      (other 1))))"))

(defun outer-with (&rest replacements)
  "*OUTER* with each (OLD . NEW) line replacement applied, NEW a list of lines."
  (let ((ls (uiop:split-string *outer* :separator '(#\Newline))))
    (loop for (old . new) in replacements
          do (setf ls (loop for l in ls
                            if (string= l old) append new
                              else collect l)))
    (format nil "~{~A~^~%~}" ls)))

(deftest-parametrize replaced-local-definitions-indent-as-such
    ((source path op args expected)
     ;; #14 item 2
     (*outer* '(0 3 2 1 1) #'replace-node (list (lines "(other (x)" "\"Doc.\"" "(list x" "a))"))
      (outer-with '("             (other (x)" "             (other (x)" "               \"Doc.\"")))
     (*outer* '(0 3 2 1 0) #'replace-node (list (lines "(touch ()" "(setf a 3))"))
      (outer-with '("               \"Doc.\"") '("               (setf a 2))" "               (setf a 3))")))
     ((lines "(flet ((f (x)" "         (1+ x)))" "  (f 1))") '(0 1 0) #'replace-node
      (list (lines "(f (x)" "(declare (fixnum x))" "(1+ x))"))
      (lines "(flet ((f (x)" "         (declare (fixnum x))" "         (1+ x)))" "  (f 1))"))
     ((lines "(macrolet ((m (a)" "             a))" "  (m 1))") '(0 1 0) #'replace-node
      (list (lines "(m (a)" "`(list ,a" ",a))"))
      (lines "(macrolet ((m (a)" "             `(list ,a" "                    ,a)))" "  (m 1))"))
     ;; a clause of HANDLER-CASE and a (:method …) of DEFGENERIC, replaced whole
     ((lines "(handler-case (foo)" "  (error (e)" "    (print e)))") '(0 2) #'replace-node
      (list (lines "(error (e)" "(print e)" "nil)"))
      (lines "(handler-case (foo)" "  (error (e)" "    (print e)" "    nil))"))
     ((lines "(defgeneric area (shape)" "  (:method ((s square))" "    (side s)))") '(0 3) #'replace-node
      (list (lines "(:method ((s square))" "(* (side s)" "(side s)))"))
      (lines "(defgeneric area (shape)" "  (:method ((s square))" "    (* (side s)" "       (side s))))")))
  (check-edit source path op args expected))

(deftest-parametrize inserted-forms-get-their-own-line
    ((source path op args expected)
     ;; #15: before / after the first binding, which shares the LABELS line
     (*outer* '(0 3 2 1 0) #'insert-before (list (lines "(route-hop ()" "\"Doc.\"" "1)"))
      (outer-with '("    (labels ((touch ()"
                    "    (labels ((route-hop ()" "               \"Doc.\"" "               1)"
                    "             (touch ()")))
     (*outer* '(0 3 2 1 0) #'insert-after (list (lines "(add-proxy-authorization (headers)" "headers)"))
      (outer-with '("               (setf a 2))" "               (setf a 2))"
                    "             (add-proxy-authorization (headers)" "               headers)")))
     ;; after the last binding, before a binding that is not the first
     (*outer* '(0 3 2 1 1) #'insert-after '("(last-one () 3)")
      (outer-with '("                     a)))" "                     a))" "             (last-one () 3))")))
     (*outer* '(0 3 2 1 1) #'insert-before (list (lines "(mid ()" "2)"))
      (outer-with '("             (other (x)" "             (mid ()" "               2)" "             (other (x)")))
     ;; the only binding of a LET: a multi-line form goes on its own line
     ((lines "(let ((a 1))" "  a)") '(0 1 0) #'insert-after (list (lines "(b (list 1" "2))"))
      (lines "(let ((a 1)" "      (b (list 1" "               2)))" "  a)"))
     ;; ...a one-line form into a one-line list stays on the line
     ((lines "(let ((a 1))" "  a)") '(0 1 0) #'insert-after '("(b 2)") (lines "(let ((a 1) (b 2))" "  a)"))
     ((lines "(let ((a 1))" "  a)") '(0 1 0) #'insert-before '("(z 0)") (lines "(let ((z 0) (a 1))" "  a)"))
     ;; first and last of a quoted list laid out one per line
     ((lines "(defparameter *xs*" "  '(alpha" "    beta))") '(0 2 0 0) #'insert-before '("zeta")
      (lines "(defparameter *xs*" "  '(zeta" "    alpha" "    beta))"))
     ((lines "(defparameter *xs*" "  '(alpha" "    beta))") '(0 2 0 1) #'insert-after '("omega")
      (lines "(defparameter *xs*" "  '(alpha" "    beta" "    omega))"))
     ;; the first form of a body that shares the operator's line
     ((lines "(progn (a 1" "          2)" "  (b))") '(0 1) #'insert-before '("(z)")
      (lines "(progn (z)" "  (a 1" "     2)" "  (b))")))
  (check-edit source path op args expected))

(deftest move-lands-on-its-own-line
  "#15: moving a binding before the first binding of another LABELS."
  (let* ((source (lines *outer* "" "(labels ((route-hop ()" "           \"Doc.\"" "           1))" "  nil)"))
         (ps (read-source source))
         (plans (move-node ps (node-at-path ps '(1 1 0)) ps (node-at-path ps '(0 3 2 1 0)) :where :before))
         (actual (source-string (parsed-source-text (verify-plan (first plans))))))
    (ok (equal actual
               (lines (outer-with '("    (labels ((touch ()"
                                    "    (labels ((route-hop ()" "               \"Doc.\"" "               1)"
                                    "             (touch ()"))
                      "" "(labels ()" "  nil)"))
        actual)))

(deftest-parametrize slurped-forms-keep-the-list-layout
    ((source path op args expected)
     ;; #15: a sibling slurped in as the first binding
     ((lines "(defun outer ()" "  (labels (route-hop ()" "            1)" "          ((touch ()" "             (setf a 2)))"
             "    (touch)))")
      '(0 3 2) #'slurp-backward '()
      (lines "(defun outer ()" "  (labels ((route-hop ()" "             1)" "           (touch ()" "             (setf a 2)))"
             "    (touch)))"))
     ;; the same when the bindings sit at the body column
     ((lines "(defun outer ()" "  (labels (route-hop ()" "            1)" "    ((touch ()" "       (setf a 2)))"
             "    (touch)))")
      '(0 3 2) #'slurp-backward '()
      (lines "(defun outer ()" "  (labels ((route-hop ()" "             1)" "           (touch ()" "             (setf a 2)))"
             "    (touch)))"))
     ;; a form slurped in last goes on its own line at the body column
     ((lines "(let ((x 1))" "  (foo))" "" "(bar)") '(0) #'slurp-forward '()
      (lines "(let ((x 1))" "  (foo)" "  (bar))"))
     ((lines "(progn" "  (a))" "(when x" "  (b))") '(0) #'slurp-forward '()
      (lines "(progn" "  (a)" "  (when x" "    (b)))"))
     ;; one line stays one line
     ("(a (b) c d)" '(0 1) #'slurp-forward '() "(a (b c) d)")
     ("(a b (c) d)" '(0 2) #'slurp-backward '() "(a (b c) d)"))
  (check-edit source path op args expected))
