(in-package #:lisp-source-protocol)

(defvar *source-features* nil
  "Feature list used for #+/#- while reading. NIL means the host *FEATURES*.
Elements are keywords; comparison is by name, so no symbols are interned.")

(defclass source-client (eclector.parse-result:parse-result-client)
  ((text :initarg :text :reader client-text)
   (offset :initarg :offset :initform 0 :accessor client-offset
           :documentation "Offset of the current segment stream in the full text.")
   (features :initarg :features :reader client-features))
  (:documentation "Eclector client that builds NODEs, never interns or evaluates."))

(defmethod eclector.base:source-position ((client source-client) stream)
  (+ (client-offset client) (file-position stream)))

(defmethod eclector.base:make-source-range ((client source-client) start end)
  (cons start end))

;;; Reading never evaluates: #. reads its form and yields a placeholder.
(defmethod eclector.reader:state-value ((client source-client) (aspect (eql '*read-eval*)))
  t)

(defmethod eclector.reader:evaluate-expression ((client source-client) expression)
  (make-read-eval-placeholder expression))

(defmethod eclector.reader:make-structure-instance ((client source-client) name initargs)
  (make-structure-placeholder name initargs))

(defmethod eclector.reader:interpret-symbol
    ((client source-client) stream package-indicator symbol-name internp)
  (declare (ignore stream))
  (make-symbol-token package-indicator symbol-name
                     (and (stringp package-indicator) internp)))

;;; Character names: standard names first, then the host's names (#\Nul,
;;; #\Esc) and U+XXXX hex names (#﻿), so implementation names read cleanly.
(defmethod eclector.reader:find-character ((client source-client) (designator string))
  (or (call-next-method)
      (name-char designator)
      (let ((n (length designator)))
        (when (and (<= 2 n 7) (char-equal (char designator 0) #\u))
          (let ((code (parse-integer designator :start 1 :radix 16 :junk-allowed t)))
            (when (and code (< code char-code-limit)
                       (every (lambda (c) (digit-char-p c 16)) (subseq designator 1)))
              (code-char code)))))))

;;; Feature expressions are evaluated by name against the client's feature list.
(defmethod eclector.reader:check-feature-expression ((client source-client) expression)
  (declare (ignore expression))
  t)

(defun feature-name (x)
  (cond ((symbol-token-p x) (symbol-token-name x))
        ((symbolp x) (symbol-name x))
        (t nil)))

(defmethod eclector.reader:evaluate-feature-expression ((client source-client) expression)
  (let ((features (client-features client)))
    (labels ((eval-fx (x)
               (if (consp x)
                   (let ((op (feature-name (first x))))
                     (cond ((string= op "AND") (every #'eval-fx (rest x)))
                           ((string= op "OR") (some #'eval-fx (rest x)))
                           ((string= op "NOT") (not (eval-fx (second x))))
                           (t nil)))
                   (let ((name (feature-name x)))
                     (and name (member name features :key #'symbol-name :test #'string=)
                          t)))))
      (eval-fx expression))))

;;; Parse results

(defun classify-expression (text start value)
  (let* ((string (source-string text))
         (c0 (char string start))
         (c1 (when (< (1+ start) (length string)) (char string (1+ start)))))
    (case c0
      (#\( :list)
      (#\' :quote)
      (#\` :quasiquote)
      (#\, (if (eql c1 #\@) :unquote-splicing :unquote))
      (#\" :string)
      (#\# (case c1
             (#\' :function)
             (#\( :vector)
             (#\\ :character)
             ((#\+ #\-) :feature-conditional)
             (#\: :symbol)
             (t :dispatch)))
      (t (typecase value
           (symbol-token :symbol)
           (number :number)
           (t :token))))))

(defmethod eclector.parse-result:make-expression-result
    ((client source-client) result children source)
  (let ((text (client-text client)))
    (make-instance 'expression-node
                   :kind (classify-expression text (car source) result)
                   :start (car source) :end (cdr source)
                   :value result :children children :source text)))

(defmethod eclector.parse-result:make-expression-result
    ((client source-client) (result eclector.parse-result:definition) children source)
  (make-instance 'expression-node
                 :kind :label-definition :start (car source) :end (cdr source)
                 :children children :source (client-text client)))

(defmethod eclector.parse-result:make-expression-result
    ((client source-client) (result eclector.parse-result:reference) children source)
  (make-instance 'expression-node
                 :kind :label-reference :start (car source) :end (cdr source)
                 :children children :source (client-text client)))

(defun skipped-kind (reason)
  (let ((head (if (consp reason) (car reason) reason)))
    (case head
      (:line-comment :comment)
      (:block-comment :block-comment)
      ((:sharpsign-plus :sharpsign-minus) :feature-skipped)
      (:reader-macro :reader-macro)
      (t :suppressed))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client source-client) stream reason children source)
  (declare (ignore stream))
  (make-instance 'trivia-node
                 :kind (let ((s (source-string (client-text client))))
                         (if (and (< (1+ (car source)) (length s))
                                  (char= (char s (car source)) #\#)
                                  (char= (char s (1+ (car source))) #\|))
                             :block-comment ; also when recovery skipped setting the reason
                             (skipped-kind reason)))
                 :start (car source) :end (cdr source)
                 :value reason :children children :source (client-text client)))
