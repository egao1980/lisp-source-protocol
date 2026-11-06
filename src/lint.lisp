(in-package #:lisp-source-protocol)

;;; Lint: static rules over a PARSED-SOURCE. Each rule is an EQL method on
;;; RUN-LINT-RULE returning DIAGNOSTICs. Add a rule by defining a method and
;;; pushing its keyword onto *LINT-RULES*.

(defvar *lint-rules*
  '(:unused-variable :missing-in-package :undefined-export :missing-docstring
    :asd-missing-component)
  "Rules LINT runs by default.")

(defclass lint-context ()
  ((model :initarg :model :reader lint-context-model)
   (parsed :initarg :parsed :reader lint-context-parsed)))

(defgeneric run-lint-rule (rule parsed context)
  (:documentation "Diagnostics for RULE over PARSED."))

(defun make-lint-diagnostic (kind severity node format-control &rest args)
  (make-instance 'diagnostic :kind kind :severity severity
                             :message (apply #'format nil format-control args)
                             :start (node-start node) :end (node-end node)))

(defun lint (parsed &key (rules *lint-rules*) model)
  "Run RULES over PARSED. MODEL defaults to a package model over PARSED alone."
  (let ((context (make-instance 'lint-context
                                :model (or model (build-package-model (list parsed)))
                                :parsed parsed)))
    (sort (loop for rule in rules append (run-lint-rule rule parsed context))
          #'< :key #'diagnostic-start)))

;;; :unused-variable

(defparameter *lambda-list-keywords*
  '("&OPTIONAL" "&REST" "&BODY" "&KEY" "&AUX" "&ALLOW-OTHER-KEYS" "&WHOLE" "&ENVIRONMENT"))

(defun lambda-list-bindings (ll &key specialized)
  "Variable name nodes bound by lambda-list node LL. With SPECIALIZED
(DEFMETHOD), skip required parameters: methods treat them as ignorable."
  (let ((out '())
        (section :required))
    (dolist (item (node-forms ll) (nreverse out))
      (let ((name (node-symbol-name item)))
        (cond ((and name (member name *lambda-list-keywords* :test #'string=))
               (setf section (intern (subseq name 1) :keyword)))
              ((eq (node-kind item) :symbol)
               (unless (and specialized (eq section :required))
                 (push item out)))
              ((eq (node-kind item) :list)
               (let ((parts (node-forms item)))
                 (case section
                   (:required
                    ;; DEFMETHOD required parameters are ignorable (CLHS 7.6.4).
                    (unless specialized
                      ;; destructuring (DEFMACRO) sub-list
                      (setf out (revappend (lambda-list-bindings item) out))))
                   ((:optional :key :aux)
                    (let ((var (first parts)))
                      (cond ((and var (eq (node-kind var) :symbol)) (push var out))
                            ((and var (eq (node-kind var) :list))
                             (alexandria:when-let ((v (second (node-forms var))))
                               (when (eq (node-kind v) :symbol) (push v out))))))
                    (alexandria:when-let ((supplied (third parts)))
                      (when (eq (node-kind supplied) :symbol) (push supplied out))))
                   (t nil)))))))))

(defun declared-ignored-names (body)
  "Names in (declare (ignore …)) / (declare (ignorable …)) at the start of BODY."
  (let ((out '()))
    (dolist (form body out)
      (unless (equal (node-head-name form) "DECLARE") (return out))
      (dolist (spec (rest (node-forms form)))
        (when (member (node-head-name spec) '("IGNORE" "IGNORABLE") :test #'equal)
          (dolist (v (rest (node-forms spec)))
            (alexandria:when-let ((n (or (node-symbol-name v)
                                         ;; (ignore (function foo)) / #'foo
                                         (and (eq (node-kind v) :function)
                                              (node-symbol-name (first (node-forms v)))))))
              (push n out))))))))

(defun mentions-name-p (nodes name exclude)
  "True when some :SYMBOL node named NAME other than EXCLUDE appears in NODES."
  (walk-nodes (lambda (n)
                (when (and (not (eq n exclude))
                           (eq (node-kind n) :symbol)
                           (eq (symbol-token-package (node-value n)) :current)
                           (string= (symbol-token-name (node-value n)) name))
                  (return-from mentions-name-p t)))
              nodes)
  nil)

(defun check-bindings (bindings scope)
  "Unused-variable diagnostics for BINDINGS (name nodes) given SCOPE forms."
  (let ((ignored (declared-ignored-names scope)))
    (loop for b in bindings
          for name = (node-symbol-name b)
          unless (or (null name)
                     ;; binding a *special* is used by the dynamic extent
                     (and (> (length name) 1) (char= (char name 0) #\*)
                          (char= (char name (1- (length name))) #\*))
                     (member name ignored :test #'string=)
                     (mentions-name-p scope name b))
            collect (make-lint-diagnostic :unused-variable :warning b
                                          "Variable ~A is never used" name))))

(defun binding-form-diagnostics (form)
  "Unused bindings introduced directly by FORM."
  (let* ((head (node-head-name form))
         (forms (node-forms form)))
    (flet ((ll-after (start-index &key specialized)
             (let* ((rest (nthcdr start-index forms))
                    (ll (find :list rest :key #'node-kind)))
               (when ll
                 (check-bindings (lambda-list-bindings ll :specialized specialized)
                                 (rest (member ll forms)))))))
      (cond
        ((null head) nil)
        ((member head '("DEFUN" "DEFMACRO") :test #'string=) (ll-after 2))
        ((string= head "DEFMETHOD") (ll-after 2 :specialized t))
        ((string= head "LAMBDA") (ll-after 1))
        ((member head '("LET" "LET*") :test #'string=)
         (let ((bindings-node (second forms)))
           (when (and bindings-node (eq (node-kind bindings-node) :list))
             (let ((specs (node-forms bindings-node)))
               (loop for spec in specs
                     for tail on specs
                     for var = (if (eq (node-kind spec) :list) (first (node-forms spec)) spec)
                     when (and var (eq (node-kind var) :symbol))
                       append (check-bindings
                               (list var)
                               (append (cddr forms)
                                       (when (string= head "LET*")
                                         (mapcar (lambda (s)
                                                   (if (eq (node-kind s) :list) s
                                                       (second (node-forms s))))
                                                 (remove nil (rest tail)))))))))))
        ((member head '("FLET" "LABELS") :test #'string=)
         (let ((defs (second forms)))
           (when (and defs (eq (node-kind defs) :list))
             (loop for def in (node-forms defs)
                   for parts = (and (eq (node-kind def) :list) (node-forms def))
                   for ll = (second parts)
                   when (and ll (eq (node-kind ll) :list))
                     append (check-bindings (lambda-list-bindings ll) (cddr parts))))))
        ((string= head "DESTRUCTURING-BIND")
         (let ((ll (second forms)))
           (when (and ll (eq (node-kind ll) :list))
             (check-bindings (lambda-list-bindings ll) (cdddr forms)))))
        ((string= head "MULTIPLE-VALUE-BIND")
         (let ((vars (second forms)))
           (when (and vars (eq (node-kind vars) :list))
             (check-bindings (remove :symbol (node-forms vars) :key #'node-kind :test-not #'eq)
                             (cdddr forms)))))
        (t nil)))))

(defmethod run-lint-rule ((rule (eql :unused-variable)) parsed context)
  (declare (ignore context))
  (let ((out '()))
    (labels ((visit (node)
               ;; Quoted data and backquote templates are not code.
               (unless (member (node-kind node) '(:quote :quasiquote :feature-skipped))
                 (when (eq (node-kind node) :list)
                   (setf out (revappend (binding-form-diagnostics node) out)))
                 (mapc #'visit (node-forms node)))))
      (mapc #'visit (parsed-source-forms parsed)))
    (nreverse out)))

;;; :missing-in-package

(defun asd-source-p (parsed)
  (let ((path (parsed-source-path parsed)))
    (and path (string-equal (pathname-type path) "asd"))))

(defmethod run-lint-rule ((rule (eql :missing-in-package)) parsed context)
  (declare (ignore context))
  (let ((forms (parsed-source-forms parsed)))
    (unless (or (asd-source-p parsed)
                (source-packages parsed)
                (every (lambda (f) (member (node-head-name f) *defpackage-operators* :test #'equal))
                       forms))
      (let ((first-def (find-if (lambda (f) (definition-operator-p (node-head-name f))) forms)))
        (when first-def
          (list (make-lint-diagnostic :missing-in-package :note first-def
                                      "Definitions without a preceding IN-PACKAGE")))))))

;;; :undefined-export / :missing-docstring

(defun slot-accessor-names (form)
  "Accessor / reader / writer names of DEFCLASS / DEFINE-CONDITION slots."
  (let ((slots (fourth (node-forms form)))
        (out '()))
    (when (and slots (eq (node-kind slots) :list))
      (dolist (slot (node-forms slots))
        (when (eq (node-kind slot) :list)
          (loop for (key value) on (rest (node-forms slot)) by #'cddr
                when (and value (member (node-symbol-name key) '("ACCESSOR" "READER" "WRITER")
                                        :test #'equal))
                  do (alexandria:when-let ((n (or (node-symbol-name value)
                                                  ;; (setf foo) writer
                                                  (and (eq (node-kind value) :list)
                                                       (node-symbol-name (second (node-forms value)))))))
                       (push n out))))))
    out))

(defun struct-defined-names (form)
  (let* ((spec (second (node-forms form)))
         (name (if (eq (node-kind spec) :list) (node-symbol-name (first (node-forms spec)))
                   (node-symbol-name spec))))
    (when name
      (list* name (format nil "MAKE-~A" name) (format nil "~A-P" name) (format nil "COPY-~A" name)
             (loop for slot in (cddr (node-forms form))
                   for slot-name = (if (eq (node-kind slot) :list)
                                       (node-symbol-name (first (node-forms slot)))
                                       (node-symbol-name slot))
                   when slot-name collect (format nil "~A-~A" name slot-name))))))

(defun defined-names (sources)
  "Upcased names defined across SOURCES: any def* form at any depth outside
quoted data (so (unless (boundp ...) (defconstant ...)) counts), slot
accessors, DEFSTRUCT functions, and any :ACCESSOR / :READER / :WRITER value."
  (let ((names (make-hash-table :test #'equal)))
    (labels ((note (n) (when n (setf (gethash n names) t)))
             (visit (node)
               (unless (member (node-kind node) '(:quote :feature-skipped))
                 (when (eq (node-kind node) :list)
                   (let* ((forms (node-forms node))
                          (head (node-head-name node)))
                     (when (definition-operator-p head)
                       (let ((name (definition-name-node node)))
                         (when name
                           (note (node-symbol-name name))
                           (when (eq (node-kind name) :list)
                             (note (node-symbol-name (first (node-forms name))))
                             (note (node-symbol-name (second (node-forms name)))))))
                       (cond ((member head '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
                              (mapc #'note (slot-accessor-names node)))
                             ((string= head "DEFSTRUCT")
                              (mapc #'note (struct-defined-names node)))))
                     (loop for (key value) on forms
                           when (and value
                                     (eq (node-kind key) :symbol)
                                     (eq (symbol-token-package (node-value key)) :keyword)
                                     (member (symbol-token-name (node-value key))
                                             '("ACCESSOR" "READER" "WRITER") :test #'string=))
                             do (note (node-symbol-name value)))))
                 (mapc #'visit (node-forms node)))))
      (dolist (ps sources names)
        (mapc #'visit (parsed-source-forms ps))))))

(defun model-defined-names (model)
  "DEFINED-NAMES over the model's sources, computed once per model."
  (or (package-model-defined-names model)
      (setf (package-model-defined-names model)
            (defined-names (package-model-sources model)))))

(defun file-package-infos (parsed model)
  (remove-if-not (lambda (info) (eq (package-info-source info) parsed))
                 (remove-duplicates (alexandria:hash-table-values (package-model-table model)))))

(defmethod run-lint-rule ((rule (eql :undefined-export)) parsed context)
  (let* ((model (lint-context-model context))
         (defined (model-defined-names model)))
    (loop for info in (file-package-infos parsed model)
          append (loop for name in (package-info-exports info)
                       unless (gethash name defined)
                         collect (make-lint-diagnostic
                                  :undefined-export :note (package-info-node info)
                                  "~A exports ~A, which no analysed file defines"
                                  (package-info-name info) name)))))

(defmethod run-lint-rule ((rule (eql :missing-docstring)) parsed context)
  (let* ((model (lint-context-model context))
         (exports (make-hash-table :test #'equal)))
    (dolist (info (remove-duplicates (alexandria:hash-table-values (package-model-table model))))
      (dolist (name (package-info-exports info))
        (setf (gethash (cons (package-info-name info) name) exports) t)))
    (loop for entry in (outline parsed)
          when (and (member (outline-entry-kind entry)
                            '("defun" "defmacro" "defgeneric" "defclass") :test #'string=)
                    (null (outline-entry-docstring entry))
                    (gethash (cons (canonical-package-name model (or (outline-entry-package entry)
                                                                     "COMMON-LISP-USER"))
                                   (string-upcase (outline-entry-name entry)))
                             exports))
            collect (make-lint-diagnostic :missing-docstring :note (outline-entry-node entry)
                                          "Exported ~A ~A has no docstring"
                                          (outline-entry-kind entry) (outline-entry-name entry)))))

;;; :asd-missing-component

(defun plist-option (forms key)
  "Value node after keyword KEY in a flat list of option nodes."
  (loop for (k v) on forms
        when (equal (node-symbol-name k) key) do (return v)))

(defun component-diagnostics (components base)
  (loop for c in (and components (node-forms components))
        append (when (eq (node-kind c) :list)
                 (let* ((parts (node-forms c))
                        (type (node-symbol-name (first parts)))
                        (name (designator-name (second parts)))
                        (pathname (string-node-value (plist-option (cddr parts) "PATHNAME"))))
                   (when name
                     (cond ((equal type "FILE")
                            (let ((file (merge-pathnames
                                         (format nil "~A.lisp" (or pathname (string-downcase name)))
                                         base)))
                              (unless (probe-file file)
                                (list (make-lint-diagnostic :asd-missing-component :warning c
                                                            "Component file ~A does not exist"
                                                            (namestring file))))))
                           ((equal type "MODULE")
                            (component-diagnostics
                             (plist-option (cddr parts) "COMPONENTS")
                             (uiop:ensure-directory-pathname
                              (merge-pathnames (or pathname (string-downcase name)) base))))))))))

(defmethod run-lint-rule ((rule (eql :asd-missing-component)) parsed context)
  (declare (ignore context))
  (when (asd-source-p parsed)
    (let ((dir (uiop:pathname-directory-pathname (parsed-source-path parsed))))
      (loop for form in (parsed-source-forms parsed)
            when (equal (node-head-name form) "DEFSYSTEM")
              append (let* ((options (cddr (node-forms form)))
                            (pathname (string-node-value (plist-option options "PATHNAME"))))
                       (component-diagnostics
                        (plist-option options "COMPONENTS")
                        (if pathname
                            (uiop:ensure-directory-pathname (merge-pathnames pathname dir))
                            dir)))))))
