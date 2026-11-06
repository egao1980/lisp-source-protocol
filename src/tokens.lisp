(in-package #:lisp-source-protocol)

(defstruct (symbol-token (:constructor make-symbol-token (package name internal-p)))
  "A symbol as written in source. Never interned.
PACKAGE is NIL (uninterned #:foo), :CURRENT, :KEYWORD or the package name string."
  (package nil :read-only t)
  (name "" :type string :read-only t)
  (internal-p nil :read-only t))

(defmethod print-object ((token symbol-token) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (token stream :type t)
        (format stream "~A~A~A"
                (case (symbol-token-package token)
                  ((nil) "#")
                  (:current "")
                  (:keyword "")
                  (t (symbol-token-package token)))
                (case (symbol-token-package token)
                  (:current "")
                  ((nil :keyword) ":")
                  (t (if (symbol-token-internal-p token) "::" ":")))
                (symbol-token-name token)))))

(defstruct (read-eval-placeholder (:constructor make-read-eval-placeholder (form)))
  "Stands in for the value of a #. form, which is never evaluated."
  (form nil :read-only t))

(defstruct (structure-placeholder (:constructor make-structure-placeholder (name initargs)))
  "Stands in for a #S(...) instance, which is never constructed."
  (name nil :read-only t)
  (initargs nil :read-only t))

(defun token-name= (token name)
  (and (symbol-token-p token)
       (string-equal (symbol-token-name token) name)))
