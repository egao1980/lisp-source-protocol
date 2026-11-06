(in-package #:lisp-source-protocol)

(defclass outline-entry ()
  ((kind :initarg :kind :reader outline-entry-kind
         :documentation "Operator name, downcased string: \"defun\", \"defclass\", \"in-package\", …")
   (name :initarg :name :reader outline-entry-name)
   (node :initarg :node :reader outline-entry-node)
   (docstring :initarg :docstring :initform nil :reader outline-entry-docstring)
   (lambda-list :initarg :lambda-list :initform nil :reader outline-entry-lambda-list
                :documentation "Source text of the lambda list, when the form has one.")
   (package :initarg :package :initform nil :reader outline-entry-package
            :documentation "Package name from the closest preceding IN-PACKAGE.")))

(defmethod print-object ((e outline-entry) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~(~A~) ~A" (outline-entry-kind e) (outline-entry-name e))))

(defparameter *lambda-list-operators*
  '("DEFUN" "DEFMACRO" "DEFGENERIC" "DEFMETHOD" "DEFTYPE" "DEFINE-COMPILER-MACRO"
    "DEFINE-MODIFY-MACRO" "DEFSETF"))

(defparameter *body-docstring-operators*
  '("DEFUN" "DEFMACRO" "DEFMETHOD" "DEFTYPE" "DEFINE-COMPILER-MACRO"))

(defparameter *value-docstring-operators*
  '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFINE-SYMBOL-MACRO"))

(defparameter *option-docstring-operators*
  '("DEFCLASS" "DEFINE-CONDITION" "DEFGENERIC" "DEFPACKAGE" "DEFINE-PACKAGE"))

(defun definition-operator-p (head)
  (and head
       (or (and (>= (length head) 3) (string= head "DEF" :end1 3))
           (search "DEFINE-" head))))

(defun string-node-value (node)
  (and node (eq (node-kind node) :string) (stringp (node-value node)) (node-value node)))

(defun form-docstring (head forms)
  (cond ((member head *body-docstring-operators* :test #'string=)
         (let* ((after (if (string= head "DEFMETHOD")
                           (member :list (cddr forms) :key #'node-kind)
                           (cddr forms)))
                (body (rest after)))
           (and (rest body) (string-node-value (first body)))))
        ((member head *value-docstring-operators* :test #'string=)
         (string-node-value (fourth forms)))
        ((string= head "DEFSTRUCT")
         (string-node-value (third forms)))
        ((or (member head *option-docstring-operators* :test #'string=)
             (search "DEFINE-PACKAGE" head))
         (loop for option in (cddr forms)
               when (and (eq (node-kind option) :list)
                         (equalp (node-symbol-name (first (node-forms option))) "DOCUMENTATION"))
                 do (return (string-node-value (second (node-forms option))))))))

(defun form-lambda-list (head forms)
  (when (member head *lambda-list-operators* :test #'string=)
    (let ((ll (find :list (cddr forms) :key #'node-kind)))
      (and ll (node-text ll)))))

(defun package-designator-name (node)
  (when node
    (case (node-kind node)
      (:symbol (symbol-token-name (node-value node)))
      (:string (node-value node)))))

(defun outline (parsed)
  "Top-level definitions of PARSED, descending into PROGN / EVAL-WHEN / LOCALLY."
  (let ((entries '())
        (package nil))
    (labels ((visit (form)
               (let ((head (node-head-name form))
                     (forms (node-forms form)))
                 (cond ((null head))
                       ((member head '("PROGN" "EVAL-WHEN" "LOCALLY") :test #'string=)
                        (mapc #'visit (rest forms)))
                       ((string= head "IN-PACKAGE")
                        (setf package (package-designator-name (second forms)))
                        (push (make-instance 'outline-entry :kind "in-package"
                                                            :name package :node form)
                              entries))
                       ((definition-operator-p head)
                        (push (make-instance 'outline-entry
                                             :kind (string-downcase head)
                                             :name (or (definition-name-string form) "")
                                             :node form
                                             :docstring (form-docstring head forms)
                                             :lambda-list (form-lambda-list head forms)
                                             :package package)
                              entries))))))
      (mapc #'visit (parsed-source-forms parsed)))
    (nreverse entries)))
