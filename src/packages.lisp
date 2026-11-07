(in-package #:lisp-source-protocol)

;;; Static package model: DEFPACKAGE / UIOP:DEFINE-PACKAGE forms read as
;;; data, IN-PACKAGE tracked per offset. Nothing is loaded or created.

(defclass package-info ()
  ((name :initarg :name :reader package-info-name)
   (nicknames :initarg :nicknames :initform '() :accessor package-info-nicknames)
   (use :initarg :use :initform '() :accessor package-info-use)
   (exports :initarg :exports :initform '() :accessor package-info-exports)
   (shadows :initarg :shadows :initform '() :accessor package-info-shadows)
   (imports :initarg :imports :initform '() :accessor package-info-imports
            :documentation "Alist of symbol name → package name (:import-from, :shadowing-import-from).")
   (local-nicknames :initarg :local-nicknames :initform '() :accessor package-info-local-nicknames
                    :documentation "Alist of nickname → package name.")
   (node :initarg :node :initform nil :reader package-info-node)
   (source :initarg :source :initform nil :reader package-info-source))
  (:documentation "A package as declared in source. Names are upcased strings."))

(defmethod print-object ((p package-info) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A (~D exports)" (package-info-name p) (length (package-info-exports p)))))

(defclass package-model ()
  ((packages :initform (make-hash-table :test #'equal) :reader package-model-table
             :documentation "Name and nickname → PACKAGE-INFO.")
   (sources :initarg :sources :initform '() :accessor package-model-sources)
   (defined-names :initform nil :accessor package-model-defined-names
                  :documentation "Cache for lint: names defined across SOURCES."))
  (:documentation "Packages declared across a set of PARSED-SOURCEs."))

(defun package-model-packages (model)
  "Distinct PACKAGE-INFOs declared in MODEL's sources, sorted by name."
  (sort (remove-duplicates (alexandria:hash-table-values (package-model-table model)))
        #'string< :key #'package-info-name))

(defparameter *defpackage-operators*
  '("DEFPACKAGE" "DEFINE-PACKAGE"))

(defun designator-name (node)
  "Upcased name of a string designator node (symbol, keyword, #:sym, string, char)."
  (when node
    (case (node-kind node)
      (:symbol (symbol-token-name (node-value node)))
      (:string (let ((v (node-value node))) (and (stringp v) (string-upcase v))))
      (:character (string (node-value node)))
      (t nil))))

(defun option-name (option)
  (and (eq (node-kind option) :list)
       (designator-name (first (node-forms option)))))

(defparameter *package-options*
  '(:nicknames :use :mix :use-reexport :mix-reexport :export :reexport :shadow
    :import-from :shadowing-import-from :local-nicknames)
  "DEFPACKAGE / DEFINE-PACKAGE options the model understands.")

(defun option-keyword (name)
  "Known option keyword for NAME, without interning anything new."
  (and name (find name *package-options* :key #'symbol-name :test #'string=)))

(defun parse-package-form (form source)
  (let* ((forms (node-forms form))
         (info (make-instance 'package-info :name (designator-name (second forms))
                                            :node form :source source)))
    (dolist (option (cddr forms) info)
      (let ((args (rest (node-forms option))))
        (flet ((names () (remove nil (mapcar #'designator-name args))))
          (case (option-keyword (option-name option))
            (:nicknames (alexandria:appendf (package-info-nicknames info) (names)))
            ((:use :mix :use-reexport :mix-reexport)
             (alexandria:appendf (package-info-use info) (names)))
            ((:export :reexport)
             (alexandria:appendf (package-info-exports info) (names)))
            (:shadow (alexandria:appendf (package-info-shadows info) (names)))
            ((:import-from :shadowing-import-from)
             (let ((from (designator-name (first args))))
               (dolist (n (rest args))
                 (alexandria:when-let ((name (designator-name n)))
                   (push (cons name from) (package-info-imports info))))
               (when (string= (option-name option) "SHADOWING-IMPORT-FROM")
                 (alexandria:appendf (package-info-shadows info)
                                     (remove nil (mapcar #'designator-name (rest args)))))))
            (:local-nicknames
             (dolist (pair args)
               (when (eq (node-kind pair) :list)
                 (let ((p (node-forms pair)))
                   (when (and (designator-name (first p)) (designator-name (second p)))
                     (push (cons (designator-name (first p)) (designator-name (second p)))
                           (package-info-local-nicknames info)))))))))))))

(defun package-forms (parsed)
  "DEFPACKAGE-like forms at top level (also inside PROGN / EVAL-WHEN)."
  (let ((out '()))
    (labels ((visit (form)
               (let ((head (node-head-name form)))
                 (cond ((null head))
                       ((member head '("PROGN" "EVAL-WHEN" "LOCALLY") :test #'string=)
                        (mapc #'visit (rest (node-forms form))))
                       ((member head *defpackage-operators* :test #'string=)
                        (when (designator-name (second (node-forms form)))
                          (push form out)))))))
      (mapc #'visit (parsed-source-forms parsed)))
    (nreverse out)))

(defun build-package-model (sources)
  "Package model over a list of PARSED-SOURCEs."
  (let ((model (make-instance 'package-model :sources sources)))
    (dolist (ps sources model)
      (dolist (form (package-forms ps))
        (let ((info (parse-package-form form ps)))
          (dolist (name (cons (package-info-name info) (package-info-nicknames info)))
            (setf (gethash name (package-model-table model)) info)))))))

(defun find-package-info (model name)
  (and name (gethash (string-upcase name) (package-model-table model))))

(defun canonical-package-name (model name)
  "Canonical name for NAME: declared packages through the model, host
packages through FIND-PACKAGE (lookup only), otherwise NAME itself."
  (let ((info (find-package-info model name)))
    (cond (info (package-info-name info))
          ((find-package name) (package-name (find-package name)))
          (t name))))

(defun source-packages (parsed)
  "Sorted list of (offset . package-name) from top-level IN-PACKAGE forms."
  (let ((out '()))
    (dolist (form (parsed-source-forms parsed) (nreverse out))
      (when (equal (node-head-name form) "IN-PACKAGE")
        (alexandria:when-let ((name (designator-name (second (node-forms form)))))
          (push (cons (node-end form) name) out))))))

(defun package-at (parsed offset &optional (default "COMMON-LISP-USER"))
  "Package in effect at OFFSET according to preceding IN-PACKAGE forms."
  (let ((name default))
    (loop for (end . pkg) in (source-packages parsed)
          while (<= end offset)
          do (setf name pkg))
    name))

(defun package-exports-p (model package-name symbol-name)
  (let ((info (find-package-info model package-name)))
    (if info
        (member symbol-name (package-info-exports info) :test #'string=)
        (let ((host (find-package package-name)))
          (and host (eq (nth-value 1 (find-symbol symbol-name host)) :external))))))

(defun resolve-symbol-token (model token context-package)
  "Return (values home-package-name symbol-name) for TOKEN read in
CONTEXT-PACKAGE. Home is NIL for an uninterned #:symbol."
  (let ((name (symbol-token-name token))
        (pkg (symbol-token-package token)))
    (cond ((null pkg) (values nil name))
          ((eq pkg :keyword) (values "KEYWORD" name))
          ((stringp pkg)
           (let* ((ctx (find-package-info model context-package))
                  (local (and ctx (cdr (assoc pkg (package-info-local-nicknames ctx)
                                              :test #'string=)))))
             (resolve-in-package model (canonical-package-name model (or local pkg)) name)))
          (t (resolve-in-package model (canonical-package-name model context-package) name)))))

(defun resolve-in-package (model package-name name)
  "Home package of NAME accessible in PACKAGE-NAME."
  (let ((info (find-package-info model package-name)))
    (cond ((null info)
           (let ((host (find-package package-name)))
             (if host
                 (let ((sym (find-symbol name host)))
                   (values (if sym
                               (let ((home (symbol-package sym)))
                                 (if home (package-name home) package-name))
                               package-name)
                           name))
                 (values package-name name))))
          ((member name (package-info-shadows info) :test #'string=)
           (let ((import (assoc name (package-info-imports info) :test #'string=)))
             (values (if import (canonical-package-name model (cdr import)) package-name) name)))
          ((assoc name (package-info-imports info) :test #'string=)
           (values (canonical-package-name
                    model (cdr (assoc name (package-info-imports info) :test #'string=)))
                   name))
          (t
           (let ((from (find-if (lambda (used)
                                  (package-exports-p model (canonical-package-name model used) name))
                                (package-info-use info))))
             (if from
                 (resolve-in-package model (canonical-package-name model from) name)
                 (values package-name name)))))))

(defun node-symbol-identity (model parsed node)
  "(values package name) for a :SYMBOL node, resolved in its file context."
  (when (and (eq (node-kind node) :symbol) (symbol-token-p (node-value node)))
    (resolve-symbol-token model (node-value node) (package-at parsed (node-start node)))))

(defun package-option-references (model ps pkg name)
  "String-designator nodes naming PKG::NAME inside package forms of PS:
:EXPORT / :SHADOW / :INTERN of PKG itself, and :IMPORT-FROM PKG elsewhere."
  (let ((out '()))
    (dolist (form (package-forms ps) out)
      (let ((own (equal (canonical-package-name model (designator-name (second (node-forms form))))
                        pkg)))
        (dolist (option (cddr (node-forms form)))
          (let ((key (option-name option))
                (args (rest (node-forms option))))
            (flet ((collect (nodes)
                     (dolist (n nodes)
                       (when (equal (designator-name n) name)
                         (push n out)))))
              (cond ((and own (member key '("EXPORT" "SHADOW" "INTERN") :test #'equal))
                     (collect args))
                    ((and (member key '("IMPORT-FROM" "SHADOWING-IMPORT-FROM") :test #'equal)
                          (equal (canonical-package-name model (designator-name (first args))) pkg))
                     (collect (rest args)))))))))))

(defun find-references (model package-name symbol-name &key (sources (package-model-sources model)))
  "All nodes in SOURCES naming PACKAGE-NAME::SYMBOL-NAME: resolved :SYMBOL
nodes plus string designators in DEFPACKAGE options. Returns a list of
(parsed-source . node) in source order per file."
  (let ((pkg (canonical-package-name model (string-upcase package-name)))
        (name (string-upcase symbol-name))
        (out '()))
    (dolist (ps sources (remove-duplicates (nreverse out) :key #'cdr :from-end t))
      (dolist (n (package-option-references model ps pkg name))
        (push (cons ps n) out))
      (walk-nodes (lambda (n)
                    (when (and (eq (node-kind n) :symbol)
                               (symbol-token-p (node-value n))
                               (string= (symbol-token-name (node-value n)) name))
                      (multiple-value-bind (home sym) (node-symbol-identity model ps n)
                        (when (and (equal home pkg) (string= sym name))
                          (push (cons ps n) out)))))
                  (parsed-source-nodes ps)))))

(defun find-definitions (model package-name symbol-name &key (sources (package-model-sources model)))
  "Outline entries across SOURCES whose name resolves to PACKAGE-NAME::SYMBOL-NAME.
Returns a list of (parsed-source . outline-entry)."
  (let ((pkg (canonical-package-name model (string-upcase package-name)))
        (name (string-upcase symbol-name))
        (out '()))
    (dolist (ps sources (nreverse out))
      (dolist (entry (outline ps))
        (let ((name-node (definition-name-node (outline-entry-node entry))))
          (when (and name-node (eq (node-kind name-node) :symbol)
                     (string= (symbol-token-name (node-value name-node)) name))
            (multiple-value-bind (home) (node-symbol-identity model ps name-node)
              (when (equal home pkg)
                (push (cons ps entry) out)))))))))
