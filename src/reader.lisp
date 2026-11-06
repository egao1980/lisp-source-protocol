(in-package #:lisp-source-protocol)

(defclass parsed-source ()
  ((text :initarg :text :reader parsed-source-text)
   (root :initarg :root :reader parsed-source-root)
   (diagnostics :initarg :diagnostics :reader parsed-source-diagnostics)
   (imbalances :initarg :imbalances :reader parsed-source-imbalances)
   (path :initarg :path :initform nil :reader parsed-source-path)
   (hash :initarg :hash :reader parsed-source-hash))
  (:documentation "Result of READ-SOURCE: a :ROOT node over the whole text."))

(defun parsed-source-nodes (ps)
  "All top-level nodes, trivia included, in source order."
  (node-children (parsed-source-root ps)))

(defun parsed-source-forms (ps)
  "Top-level expression nodes."
  (node-forms (parsed-source-root ps)))

(defun parsed-source-clean-p (ps)
  "True when the text read without errors or imbalances."
  (and (null (parsed-source-imbalances ps))
       (notany (lambda (d) (eq (diagnostic-severity d) :error))
               (parsed-source-diagnostics ps))))

(defmethod print-object ((ps parsed-source) stream)
  (print-unreadable-object (ps stream :type t)
    (format stream "~@[~A ~]~D forms ~D diagnostics ~D imbalances"
            (parsed-source-path ps) (length (parsed-source-forms ps))
            (length (parsed-source-diagnostics ps)) (length (parsed-source-imbalances ps)))))

;;; Diagnostics from Eclector conditions

(defun condition-kind (c)
  (intern (symbol-name (type-of c)) :keyword))

(defun condition-diagnostic (c fallback)
  "Diagnostic for Eclector condition C. Eclector positions come from the
client's SOURCE-POSITION, so they are already absolute; FALLBACK is used for
plain CL reader errors."
  (multiple-value-bind (pos len)
      (if (typep c '(or eclector.base:stream-position-reader-error eclector.base:end-of-file))
          (values (+ (or (eclector.base:stream-position c) fallback)
                     (eclector.base:position-offset c))
                  (eclector.base:range-length c))
          (values fallback 1))
    (make-instance 'diagnostic
                   :severity :error
                   :kind (condition-kind c)
                   :message (princ-to-string c)
                   :start pos :end (+ pos (max len 0)))))

(defun find-cut (string start end)
  "Offset of the first #\\( at column 0 on a line after the line holding START, below END."
  (loop for i = (position #\Newline string :start start :end end)
          then (position #\Newline string :start (1+ i) :end end)
        while i
        when (and (< (1+ i) end) (char= (char string (1+ i)) #\())
          do (return (1+ i))))

(defun eof-condition-p (c)
  (typep c 'eclector.base:end-of-file))

;;; Segment reading

(defun read-segment (client text start end)
  "Read forms in [START, END). Return (values nodes diagnostics broken)
where BROKEN is (form-start . eof-conditions) for the first form that ran into
END, or NIL. On BROKEN, NODES and DIAGNOSTICS stop before that form."
  (let* ((string (source-string text))
         (stream (make-string-input-stream (subseq string start end)))
         (eof (load-time-value (make-symbol "EOF")))
         (nodes '())
         (diags '()))
    (setf (client-offset client) start)
    (loop
      (let ((form-diags '())
            (eof-conditions '())
            (read-start (+ start (file-position stream))))
        (multiple-value-bind (result orphans)
            (handler-case
                (handler-bind ((eclector.base:stream-position-reader-error
                                 (lambda (c)
                                   ;; #S checks for real symbols; ours are tokens by design.
                                   (unless (typep c '(or eclector.reader:structure-type-name-is-not-a-symbol
                                                      eclector.reader:slot-name-is-not-a-string-designator))
                                     (push (condition-diagnostic c read-start) form-diags))
                                   (when (eof-condition-p c) (push c eof-conditions))
                                   (eclector.base:recover c))))
                  (eclector.parse-result:read client stream nil eof))
              ;; Unrecoverable reader errors: keep the rest of the line range as unparsed.
              ((or eclector.base:stream-position-reader-error reader-error) (c)
                (let* ((cut (or (find-cut string read-start end) end))
                       (node (make-instance 'trivia-node :kind :unparsed
                                                         :start read-start :end cut
                                                         :value (condition-kind c)
                                                         :source text :recovered t)))
                  (push (condition-diagnostic c read-start) diags)
                  (setf diags (append form-diags diags))
                  (if (>= cut end)
                      (return (values (nreverse (cons node nodes)) diags nil))
                      (progn
                        (push node nodes)
                        (file-position stream (- cut start))
                        (values :skip nil))))))
          (cond ((eq result :skip))
                ((and (eq result eof) eof-conditions orphans)
                 ;; Trailing trivia (e.g. an unterminated #|) ran into END.
                 (let ((broken (car (last orphans))))
                   (setf nodes (revappend (butlast orphans) nodes))
                   (return (values (nreverse nodes) diags
                                   (list (node-start broken) eof-conditions form-diags broken)))))
                ((eq result eof)
                 (setf nodes (revappend orphans nodes))
                 (setf diags (append form-diags diags))
                 (return (values (nreverse nodes) diags nil)))
                ((and eof-conditions (expression-node-p result))
                 (setf nodes (revappend orphans nodes))
                 (return (values (nreverse nodes) diags
                                 (list (node-start result) eof-conditions form-diags result))))
                (t
                 (setf nodes (cons result (revappend orphans nodes)))
                 (setf diags (append form-diags diags)))))))))

(defun mark-recovered (node)
  (walk-nodes (lambda (n) (setf (node-recovered-p n) t)) (list node)))

(defun eof-imbalance (text form-node conditions end)
  (let ((kinds (mapcar #'type-of conditions))
        (form-start (node-start form-node)))
    (flet ((recovered-node-of-kind (kind)
             (let ((found nil))
               (walk-nodes (lambda (n)
                             (when (and (eq (node-kind n) kind) (= (node-end n) end))
                               (setf found n)))
                           (list form-node))
               found)))
      (cond ((member 'eclector.reader:unterminated-string kinds)
             (let ((s (recovered-node-of-kind :string)))
               (make-instance 'imbalance
                              :kind :unterminated-string :form-start form-start
                              :open-position (and s (node-start s))
                              :fix-position (and s (line-end-offset
                                                    text (offset-line-column text (node-start s))))
                              :confidence :low
                              :message "String is not terminated before the next top-level form")))
            ((member 'eclector.reader:unterminated-block-comment kinds)
             (let ((b (recovered-node-of-kind :block-comment)))
               (make-instance 'imbalance
                              :kind :unterminated-block-comment :form-start form-start
                              :open-position (and b (node-start b))
                              :fix-position end :confidence :low
                              :message "#| comment is not terminated before the next top-level form")))
            (t
             (multiple-value-bind (count open fix confidence)
                 (infer-missing-close text form-start end)
               (make-instance 'imbalance
                              :kind :missing-close :form-start form-start
                              :open-position open :fix-position fix
                              :count count :confidence confidence
                              :message (format nil "~D closing parenthes~:*~[es~;is~:;es~] missing" count))))))))

(defun extra-close-imbalances (text nodes diags)
  "Stray top-level ) diagnostics, explained by the column-0 form before them.
An early ) leaves the rest of its form as top-level forms that do not start
at column 0, so the damage spans [anchor start, stray)."
  (let ((forms (remove-if-not #'expression-node-p nodes)))
    (flet ((column (offset) (nth-value 1 (offset-line-column text offset))))
      (loop for d in diags
            when (eq (diagnostic-kind d) :invalid-context-for-right-parenthesis)
              collect (let* ((stray (diagnostic-start d))
                             (before (remove-if-not (lambda (f) (<= (node-end f) stray)) forms))
                             (prev (or (find-if (lambda (f) (zerop (column (node-start f))))
                                                before :from-end t)
                                       (car (last before))))
                             (between (and prev (remove-if-not
                                                 (lambda (f) (< (node-end prev) (node-start f) stray))
                                                 forms)))
                             (indented (some (lambda (f) (plusp (column (node-start f)))) between))
                             (early (and prev (infer-extra-close text (node-start prev) stray))))
                      (cond
                        (early
                         (make-instance 'imbalance
                                        :kind :extra-close :form-start (node-start prev)
                                        :open-position stray :fix-position early
                                        :confidence :medium
                                        :message "Closing parenthesis comes too early for the indentation that follows"))
                        ((and prev indented)
                          (make-instance 'imbalance
                                         :kind :extra-close :form-start (node-start prev)
                                         :open-position stray
                                         :fix-position (1- (node-end prev))
                                         :confidence :medium
                                         :message "Form closed early; indented forms follow it before a stray )"))
                        (t
                         (make-instance 'imbalance
                                        :kind :extra-close
                                        :form-start (if prev (node-start prev) stray)
                                        :open-position stray :fix-position stray
                                        :confidence :high
                                        :message "Stray closing parenthesis"))))))))

(defun read-time-eval-diagnostics (nodes)
  (let ((out '()))
    (walk-nodes (lambda (n)
                  (when (read-eval-placeholder-p (node-value n))
                    (signal 'read-time-eval-refused :node n)
                    (push (make-instance 'diagnostic
                                         :severity :warning :kind :read-time-eval
                                         :message "#. form not evaluated"
                                         :start (node-start n) :end (node-end n))
                          out)))
                nodes)
    (nreverse out)))

(defun read-source (string &key path (features (or *source-features* *features*)))
  "Read STRING into a PARSED-SOURCE. Never evaluates and never interns.
A form that runs into end of input is re-read up to the next column-0 #\\( so
the damage stays inside that form; the result records an IMBALANCE."
  (let* ((text (make-source-text string))
         (s (source-string text))
         (n (length s))
         (client (make-instance 'source-client :text text :features features))
         (nodes '())
         (diags '())
         (imbalances '())
         (start 0))
    (loop
      (multiple-value-bind (seg-nodes seg-diags broken) (read-segment client text start n)
        (setf nodes (append nodes seg-nodes)
              diags (append diags seg-diags))
        (unless broken (return))
        (destructuring-bind (form-start conditions form-diags result) broken
          (let ((cut (find-cut s form-start n)))
            (if (null cut)
                ;; Nothing to resync on: keep Eclector's recovery to end of input.
                (progn
                  (mark-recovered result)
                  (setf nodes (append nodes (list result))
                        diags (append diags form-diags))
                  (push (eof-imbalance text result conditions n) imbalances)
                  (return))
                (multiple-value-bind (cut-nodes cut-diags cut-broken)
                    (read-segment client text form-start cut)
                  ;; Inside [form-start, cut) the form now ends at CUT.
                  (setf nodes (append nodes cut-nodes)
                        diags (append diags cut-diags))
                  (when cut-broken
                    (destructuring-bind (fs2 conds2 fdiags2 result2) cut-broken
                      (declare (ignore fs2))
                      (mark-recovered result2)
                      (setf nodes (append nodes (list result2))
                            diags (append diags fdiags2))
                      (push (eof-imbalance text result2 conds2 cut) imbalances)))
                  (setf start cut)))))))
    (setf nodes (sort nodes #'< :key #'node-start))
    (setf imbalances (append (nreverse imbalances)
                             (extra-close-imbalances text nodes diags)))
    (let ((root (make-instance 'node :kind :root :start 0 :end n
                                     :children nodes :source text)))
      (link-parents root)
      (make-instance 'parsed-source
                     :text text :root root :path path
                     :diagnostics (sort (append diags (read-time-eval-diagnostics nodes))
                                        #'< :key #'diagnostic-start)
                     :imbalances (sort imbalances #'< :key #'imbalance-form-start)
                     :hash (text-hash s)))))

(defun read-source-file (pathname &rest args &key features)
  (declare (ignore features))
  (apply #'read-source
         (alexandria:read-file-into-string pathname :external-format :utf-8)
         :path (namestring pathname) args))
