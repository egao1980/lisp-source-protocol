(in-package #:lisp-source-protocol)

(defclass node ()
  ((kind :initarg :kind :reader node-kind
         :documentation "Keyword: :list :vector :string :symbol :number :character :quote
:quasiquote :unquote :unquote-splicing :function :feature-conditional :dispatch
:label-definition :label-reference :token for expressions; :comment :block-comment
:feature-skipped :suppressed :reader-macro :unparsed for trivia.")
   (start :initarg :start :reader node-start :type fixnum)
   (end :initarg :end :reader node-end :type fixnum)
   (children :initarg :children :initform '() :accessor node-children)
   (parent :initform nil :accessor node-parent)
   (value :initarg :value :initform nil :reader node-value)
   (source :initarg :source :initform nil :accessor node-source)
   (recovered :initarg :recovered :initform nil :accessor node-recovered-p
              :documentation "True when the node was built by error recovery."))
  (:documentation "A range of source text. Children are ordered by position."))

(defclass expression-node (node) ()
  (:documentation "A node that READ would return as an object."))

(defclass trivia-node (node) ()
  (:documentation "Comments, skipped #+/#- forms and other input READ discards."))

(defun expression-node-p (x) (typep x 'expression-node))
(defun trivia-node-p (x) (typep x 'trivia-node))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S ~D..~D" (node-kind node) (node-start node) (node-end node))))

(defun node-text (node)
  (subseq (source-string (node-source node)) (node-start node) (node-end node)))

(defun node-forms (node)
  "Expression children of NODE (the elements a structural path indexes)."
  (remove-if-not #'expression-node-p (node-children node)))

(defun node-path (node)
  "Indices from the :ROOT node to NODE, counting only expression nodes.
NIL for a trivia node or the root."
  (let ((path '()))
    (do ((n node (node-parent n)))
        ((null (node-parent n)) path)
      (let ((index (position n (node-forms (node-parent n)))))
        (unless index (return nil))
        (push index path)))))

(defun node-hash (node)
  (text-hash (node-text node)))

(defun walk-nodes (function nodes)
  "Call FUNCTION on every node in NODES, depth first, pre-order."
  (labels ((walk (n)
             (funcall function n)
             (mapc #'walk (node-children n))))
    (mapc #'walk nodes)
    nil))

(defun link-parents (node)
  (dolist (child (node-children node))
    (setf (node-parent child) node)
    (link-parents child)))

(defun node-symbol-name (node)
  "Upcased symbol name when NODE is a symbol token, else NIL."
  (when node
    (let ((v (node-value node)))
      (when (and (eq (node-kind node) :symbol) (symbol-token-p v))
        (symbol-token-name v)))))

(defun node-head-name (node)
  "Symbol name of the operator of a list node, or NIL."
  (when (and node (eq (node-kind node) :list))
    (let ((head (first (node-forms node))))
      (and head (node-symbol-name head)))))

;;; Diagnostics

(defclass diagnostic ()
  ((severity :initarg :severity :reader diagnostic-severity :type (member :error :warning :note))
   (kind :initarg :kind :reader diagnostic-kind)
   (message :initarg :message :reader diagnostic-message)
   (start :initarg :start :reader diagnostic-start)
   (end :initarg :end :reader diagnostic-end)))

(defmethod print-object ((d diagnostic) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~S ~S @~D: ~A" (diagnostic-severity d) (diagnostic-kind d)
            (diagnostic-start d) (diagnostic-message d))))

(defclass imbalance ()
  ((kind :initarg :kind :reader imbalance-kind
         :type (member :missing-close :extra-close :unterminated-string :unterminated-block-comment))
   (form-start :initarg :form-start :reader imbalance-form-start
               :documentation "Start of the top-level form that contains the problem.")
   (open-position :initarg :open-position :initform nil :reader imbalance-open-position
                  :documentation "Innermost unclosed opener, or the stray closer.")
   (fix-position :initarg :fix-position :initform nil :reader imbalance-fix-position
                 :documentation "Suggested offset to insert (missing) or delete (extra) a delimiter.")
   (count :initarg :count :initform 1 :reader imbalance-count)
   (confidence :initarg :confidence :initform :medium :reader imbalance-confidence
               :type (member :high :medium :low))
   (message :initarg :message :reader imbalance-message)))

(defmethod print-object ((i imbalance) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~S x~D form@~D fix@~S ~S" (imbalance-kind i) (imbalance-count i)
            (imbalance-form-start i) (imbalance-fix-position i) (imbalance-confidence i))))
