(in-package #:lisp-source-protocol)

;;; Structural editing. An operation never writes text directly: it builds an
;;; EDIT-PLAN (non-overlapping text replacements against one PARSED-SOURCE
;;; plus the expected top-level form delta). VERIFY-PLAN re-reads the result
;;; and refuses anything that would add a reader error or imbalance.

;;; Conditions

(define-condition edit-error (lisp-source-error) ()
  (:documentation "Root of structural edit errors."))

(define-condition edit-input-unreadable (edit-error)
  ((text :initarg :text :reader edit-input-text)
   (diagnostics :initarg :diagnostics :reader edit-input-diagnostics)
   (imbalances :initarg :imbalances :initform '() :reader edit-input-imbalances))
  (:report (lambda (c s)
             (format s "Edit input does not read as balanced Lisp~@[: ~A~]"
                     (let ((d (first (edit-input-diagnostics c))))
                       (and d (diagnostic-message d)))))))

(define-condition edit-would-unbalance (edit-error)
  ((plan :initarg :plan :reader edit-error-plan)
   (diagnostics :initarg :diagnostics :reader edit-error-diagnostics)
   (imbalances :initarg :imbalances :reader edit-error-imbalances)
   (reason :initarg :reason :reader edit-error-reason))
  (:report (lambda (c s)
             (format s "Edit refused: ~A" (edit-error-reason c)))))

(define-condition stale-source (edit-error)
  ((expected :initarg :expected :reader stale-source-expected)
   (actual :initarg :actual :reader stale-source-actual)
   (path :initarg :path :initform nil :reader stale-source-path))
  (:report (lambda (c s)
             (format s "~@[~A: ~]source hash ~A does not match expected ~A; re-read first"
                     (stale-source-path c) (stale-source-actual c) (stale-source-expected c)))))

(define-condition edit-on-recovered-node (edit-error)
  ((node :initarg :node :reader edit-error-node))
  (:report (lambda (c s)
             (format s "~S was built by error recovery; repair the file or replace the whole form"
                     (edit-error-node c)))))

(define-condition edit-not-applicable (edit-error)
  ((operation :initarg :operation :reader edit-error-operation)
   (node :initarg :node :reader edit-error-node)
   (reason :initarg :reason :reader edit-error-reason))
  (:report (lambda (c s)
             (format s "~(~A~) does not apply to ~S: ~A"
                     (edit-error-operation c) (edit-error-node c) (edit-error-reason c)))))

;;; Plans

(defclass text-edit ()
  ((start :initarg :start :reader text-edit-start)
   (end :initarg :end :reader text-edit-end)
   (replacement :initarg :replacement :reader text-edit-replacement)))

(defmethod print-object ((e text-edit) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~D..~D → ~S" (text-edit-start e) (text-edit-end e) (text-edit-replacement e))))

(defun make-text-edit (start end replacement)
  (make-instance 'text-edit :start start :end end :replacement replacement))

(defclass edit-plan ()
  ((parsed :initarg :parsed :reader edit-plan-parsed)
   (edits :initarg :edits :reader edit-plan-edits)
   (form-delta :initarg :form-delta :initform 0 :reader edit-plan-form-delta
               :documentation "Expected change in the number of top-level forms.")
   (operation :initarg :operation :reader edit-plan-operation)
   (repair :initarg :repair :initform nil :reader edit-plan-repair-p
           :documentation "True for plans that must reduce imbalances rather than keep them.")))

(defmethod print-object ((p edit-plan) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~(~A~) ~D edit~:P Δforms ~@D"
            (edit-plan-operation p) (length (edit-plan-edits p)) (edit-plan-form-delta p))))

(defun apply-text-edits (string edits)
  "STRING with EDITS applied. EDITS must not overlap."
  (let ((sorted (stable-sort (copy-list edits)
                             (lambda (a b)
                               ;; at the same offset an insertion goes before a replacement
                               (or (< (text-edit-start a) (text-edit-start b))
                                   (and (= (text-edit-start a) (text-edit-start b))
                                        (= (text-edit-start a) (text-edit-end a))
                                        (/= (text-edit-start b) (text-edit-end b)))))))
        (pos 0))
    (loop for (a b) on sorted
          when (and b (> (text-edit-end a) (text-edit-start b)))
            do (error "Overlapping text edits ~S ~S" a b))
    (with-output-to-string (out)
      (dolist (e sorted)
        (write-string string out :start pos :end (text-edit-start e))
        (write-string (text-edit-replacement e) out)
        (setf pos (text-edit-end e)))
      (write-string string out :start pos))))

(defun plan-text (plan)
  (apply-text-edits (source-string (parsed-source-text (edit-plan-parsed plan)))
                    (edit-plan-edits plan)))

(defun error-count (parsed)
  (+ (length (parsed-source-imbalances parsed))
     (count :error (parsed-source-diagnostics parsed) :key #'diagnostic-severity)))

(defun verify-plan (plan &key (features (or *source-features* *features*)))
  "Re-read the plan's result. Return the new PARSED-SOURCE, or signal
EDIT-WOULD-UNBALANCE when it adds a reader error / imbalance (or, for a repair
plan, fails to remove one) or changes the top-level form count unexpectedly."
  (let* ((old (edit-plan-parsed plan))
         (new (read-source (plan-text plan) :path (parsed-source-path old) :features features))
         (old-errors (error-count old))
         (new-errors (error-count new)))
    (flet ((refuse (reason)
             (error 'edit-would-unbalance :plan plan :reason reason
                                          :diagnostics (parsed-source-diagnostics new)
                                          :imbalances (parsed-source-imbalances new))))
      (cond ((edit-plan-repair-p plan)
             (unless (< new-errors old-errors)
               (refuse (format nil "repair leaves ~D problem~:P (was ~D)" new-errors old-errors))))
            ((> new-errors old-errors)
             (refuse (format nil "result has ~D reader problem~:P (was ~D)" new-errors old-errors))))
      (when (and (zerop new-errors) (zerop old-errors))
        (let ((delta (- (length (parsed-source-forms new)) (length (parsed-source-forms old)))))
          (unless (= delta (edit-plan-form-delta plan))
            (refuse (format nil "top-level form count changed by ~@D, expected ~@D"
                            delta (edit-plan-form-delta plan))))))
      new)))

(defun check-expected-hash (parsed expected-hash)
  (when (and expected-hash (string/= expected-hash (parsed-source-hash parsed)))
    (restart-case (error 'stale-source :expected expected-hash
                                       :actual (parsed-source-hash parsed)
                                       :path (parsed-source-path parsed))
      (continue ()
        :report "Edit the current text anyway"
        nil))))

;;; Reading edit input

(defun read-edit-input (text &key (min 1) max)
  "Read TEXT as edit input. Return its top-level expression nodes, or signal
EDIT-INPUT-UNREADABLE. MIN / MAX bound the number of forms."
  (let ((ps (read-source text)))
    (unless (parsed-source-clean-p ps)
      (error 'edit-input-unreadable :text text :diagnostics (parsed-source-diagnostics ps)
                                    :imbalances (parsed-source-imbalances ps)))
    (let ((n (length (parsed-source-forms ps))))
      (when (or (< n min) (and max (> n max)))
        (error 'edit-input-unreadable
               :text text
               :diagnostics (list (make-instance 'diagnostic
                                                 :severity :error :kind :form-count
                                                 :message (format nil "expected ~D~@[..~D~] form~:P, got ~D"
                                                                  min max n)
                                                 :start 0 :end (length text))))))
    (parsed-source-forms ps)))

;;; Text helpers

(defun top-level-node-p (node)
  (let ((parent (node-parent node)))
    (and parent (eq (node-kind parent) :root))))

(defun node-string (node)
  (source-string (node-source node)))

(defun node-column (node)
  (nth-value 1 (offset-line-column (node-source node) (node-start node))))

(defun line-prefix-blank-p (node)
  "True when only whitespace precedes NODE on its line."
  (let* ((s (node-string node))
         (line-start (1+ (or (position #\Newline s :end (node-start node) :from-end t) -1))))
    (every (lambda (c) (member c '(#\Space #\Tab))) (subseq s line-start (node-start node)))))

(defvar *reindent* t
  "When true, code an edit inserts is re-indented with INDENT-CODE; when NIL,
its continuation lines are only shifted to the target column.")

(defun place-code (text column &key (input-column 0))
  "TEXT laid out to start at COLUMN."
  (if *reindent*
      (indent-code text :column column :input-column input-column)
      (indent-continuation (dedent-continuation text input-column) column)))

(defun indent-continuation (text column)
  "Shift every line of TEXT after the first right by COLUMN spaces."
  (if (zerop column)
      text
      (with-output-to-string (out)
        (let ((pad (make-string column :initial-element #\Space)))
          (loop for c across text
                do (write-char c out)
                   (when (char= c #\Newline) (write-string pad out)))))))

(defun ws-char-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun skip-ws-forward (s pos)
  (or (position-if-not #'ws-char-p s :start pos) (length s)))

(defun skip-ws-backward (s pos)
  "Offset after the last non-whitespace character before POS."
  (let ((p (position-if-not #'ws-char-p s :end pos :from-end t)))
    (if p (1+ p) 0)))

(defun sibling-forms (node)
  (node-forms (node-parent node)))

(defun next-sibling (node)
  (second (member node (sibling-forms node))))

(defun previous-sibling (node)
  (let ((forms (sibling-forms node)))
    (loop for (a b) on forms
          when (eq b node) do (return a))))

(defun ensure-editable (node operation)
  (when (node-recovered-p node)
    (restart-case (error 'edit-on-recovered-node :node node)
      (continue () :report "Edit the recovered node anyway" nil)))
  (unless (node-parent node)
    (error 'edit-not-applicable :operation operation :node node :reason "the root is not editable")))

(defun consing-dot-node-p (node)
  (and node (eq (node-kind node) :token) (string= (node-text node) ".")))

(defun dotted-list-node-p (node)
  (some #'consing-dot-node-p (node-forms node)))

(defun ensure-sibling-context (node operation)
  "NODE must be an element of a proper list, vector or the top level:
siblings under #+/#-, quote or backquote prefixes, the consing dot and
dotted lists have no free positions to edit."
  (let ((parent (node-parent node)))
    (cond ((consing-dot-node-p node)
           (error 'edit-not-applicable :operation operation :node node :reason "the consing dot"))
          ((not (member (node-kind parent) '(:list :vector :root)))
           (error 'edit-not-applicable :operation operation :node node
                                       :reason (format nil "inside a ~(~A~) prefix; edit the enclosing form"
                                                       (node-kind parent))))
          ((dotted-list-node-p parent)
           (error 'edit-not-applicable :operation operation :node node :reason "inside a dotted list")))))

(defun ensure-list-node (node operation)
  (let ((s (node-string node)))
    (unless (and (eq (node-kind node) :list)
                 (char= (char s (node-start node)) #\()
                 (char= (char s (1- (node-end node))) #\))
                 (not (dotted-list-node-p node)))
      (error 'edit-not-applicable :operation operation :node node
                                  :reason "needs a plain, proper (…) list"))))

(defun plan (parsed operation edits &key (form-delta 0) repair)
  (make-instance 'edit-plan :parsed parsed :operation operation :edits edits
                            :form-delta form-delta :repair repair))

;;; Operations. Each takes the PARSED-SOURCE the node belongs to.

(defun replace-node (parsed node new-text)
  "Replace NODE with exactly one form read from NEW-TEXT."
  (ensure-editable node :replace)
  (when (consing-dot-node-p node)
    (error 'edit-not-applicable :operation :replace :node node :reason "the consing dot"))
  (read-edit-input new-text :min 1 :max 1)
  (plan parsed :replace
        (list (make-text-edit (node-start node) (node-end node)
                              (place-code (string-trim '(#\Space #\Tab #\Newline) new-text)
                                          (node-column node))))))

(defun separator-for (node)
  "Whitespace placed between NODE and an inserted sibling."
  (cond ((top-level-node-p node) (format nil "~%~%"))
        ((line-prefix-blank-p node)
         (format nil "~%~A" (make-string (node-column node) :initial-element #\Space)))
        (t " ")))

(defun insert-after (parsed node text)
  "Insert the forms read from TEXT after NODE, as siblings."
  (ensure-editable node :insert-after)
  (ensure-sibling-context node :insert-after)
  (let ((forms (read-edit-input text)))
    (plan parsed :insert-after
          (list (make-text-edit (node-end node) (node-end node)
                                (concatenate 'string (separator-for node)
                                             (join-forms-text node forms))))
          :form-delta (if (top-level-node-p node) (length forms) 0))))

(defun insert-before (parsed node text)
  "Insert the forms read from TEXT before NODE, as siblings."
  (ensure-editable node :insert-before)
  (ensure-sibling-context node :insert-before)
  (let ((forms (read-edit-input text)))
    (plan parsed :insert-before
          (list (make-text-edit (node-start node) (node-start node)
                                (concatenate 'string (join-forms-text node forms)
                                             (separator-for node))))
          :form-delta (if (top-level-node-p node) (length forms) 0))))

(defun join-forms-text (node forms)
  (let ((sep (separator-for node))
        (column (if (top-level-node-p node) 0 (node-column node))))
    (with-output-to-string (out)
      (loop for (f . more) on forms
            do (write-string (place-code (node-text f) column :input-column (node-column f)) out)
               (when more (write-string sep out))))))

(defun delete-node (parsed node)
  "Delete NODE and the whitespace that separated it from its neighbours."
  (ensure-editable node :delete)
  (ensure-sibling-context node :delete)
  (let* ((s (node-string node))
         (start (node-start node))
         (end (node-end node))
         (after (skip-ws-forward s end))
         (closer-next (and (< after (length s)) (char= (char s after) #\))))
         (prev (previous-sibling node)))
    (multiple-value-bind (del-start del-end)
        (cond ((top-level-node-p node) (values start after))
              ((or closer-next (null (next-sibling node)))
               ;; last element: eat the whitespace before it, keep the closer in place
               (values (if prev (node-end prev) (skip-ws-backward s start)) end))
              (t (values start after)))
      (plan parsed :delete (list (make-text-edit del-start del-end ""))
            :form-delta (if (top-level-node-p node) -1 0)))))

(defun wrap-node (parsed node head)
  "Wrap NODE in a new list (HEAD NODE). HEAD may be NIL for a bare (NODE).
A multi-line NODE starts on its own line under HEAD, indented as the body of
HEAD's operator; a one-line NODE stays on HEAD's line."
  (ensure-editable node :wrap)
  (when (consing-dot-node-p node)
    (error 'edit-not-applicable :operation :wrap :node node :reason "the consing dot"))
  ;; HEAD is a prefix: operator plus any leading arguments.
  (when head (read-edit-input head :min 1))
  (let* ((head (and head (string-trim " " head)))
         (multi-line (and head *reindent* (find #\Newline (node-text node))))
         (open (cond (multi-line (format nil "(~A~%" head))
                     (head (format nil "(~A " head))
                     (t "("))))
    (plan parsed :wrap
          (if *reindent*
              (let ((column (node-column node)))
                (list (make-text-edit (node-start node) (node-end node)
                                      (indent-code (concatenate 'string open (node-text node) ")")
                                                   :column column
                                                   :input-column (if multi-line
                                                                     column
                                                                     (- column (length open)))))))
              (list (make-text-edit (node-start node) (node-start node) open)
                    (make-text-edit (node-end node) (node-end node) ")"))))))

(defun splice-node (parsed node)
  "Replace list NODE by its elements."
  (ensure-editable node :splice)
  (ensure-sibling-context node :splice)
  (ensure-list-node node :splice)
  (plan parsed :splice
        (list (make-text-edit (node-start node) (1+ (node-start node)) "")
              (make-text-edit (1- (node-end node)) (node-end node) ""))
        :form-delta (if (top-level-node-p node) (1- (length (node-forms node))) 0)))

(defun raise-node (parsed node)
  "Replace NODE's parent list with NODE."
  (ensure-editable node :raise)
  (let ((parent (node-parent node)))
    (when (or (null parent) (eq (node-kind parent) :root))
      (error 'edit-not-applicable :operation :raise :node node :reason "already top level"))
    (unless (eq (node-kind parent) :list)
      (error 'edit-not-applicable :operation :raise :node node :reason "parent is not a list"))
    (when (consing-dot-node-p node)
      (error 'edit-not-applicable :operation :raise :node node :reason "the consing dot"))
    (ensure-editable parent :raise)
    (plan parsed :raise
          (list (make-text-edit (node-start parent) (node-end parent) (node-text node))))))

(defun slurp-forward (parsed node)
  "Move the closing paren of list NODE past its next sibling."
  (ensure-editable node :slurp-forward)
  (ensure-sibling-context node :slurp-forward)
  (ensure-list-node node :slurp-forward)
  (let ((next (next-sibling node)))
    (unless next
      (error 'edit-not-applicable :operation :slurp-forward :node node :reason "no next sibling"))
    (plan parsed :slurp-forward
          (list (make-text-edit (1- (node-end node)) (node-end node) "")
                (make-text-edit (node-end next) (node-end next) ")"))
          :form-delta (if (top-level-node-p node) -1 0))))

(defun slurp-backward (parsed node)
  "Move the opening paren of list NODE before its previous sibling."
  (ensure-editable node :slurp-backward)
  (ensure-sibling-context node :slurp-backward)
  (ensure-list-node node :slurp-backward)
  (let ((prev (previous-sibling node)))
    (unless prev
      (error 'edit-not-applicable :operation :slurp-backward :node node :reason "no previous sibling"))
    (plan parsed :slurp-backward
          (list (make-text-edit (node-start prev) (node-start prev) "(")
                (make-text-edit (node-start node) (1+ (node-start node)) ""))
          :form-delta (if (top-level-node-p node) -1 0))))

(defun barf-forward (parsed node)
  "Move the last element of list NODE out after it."
  (ensure-editable node :barf-forward)
  (ensure-sibling-context node :barf-forward)
  (ensure-list-node node :barf-forward)
  (let* ((forms (node-forms node))
         (last (car (last forms)))
         (before (car (last forms 2))))
    (unless last
      (error 'edit-not-applicable :operation :barf-forward :node node :reason "empty list"))
    (let ((close-at (if (eq before last) (1+ (node-start node)) (node-end before))))
      (plan parsed :barf-forward
            (list (make-text-edit close-at close-at ")")
                  (make-text-edit (1- (node-end node)) (node-end node) ""))
            :form-delta (if (top-level-node-p node) 1 0)))))

(defun barf-backward (parsed node)
  "Move the first element of list NODE out before it."
  (ensure-editable node :barf-backward)
  (ensure-sibling-context node :barf-backward)
  (ensure-list-node node :barf-backward)
  (let* ((forms (node-forms node))
         (first (first forms))
         (second (second forms)))
    (unless first
      (error 'edit-not-applicable :operation :barf-backward :node node :reason "empty list"))
    (let ((open-at (if second (node-start second) (1- (node-end node)))))
      (plan parsed :barf-backward
            (list (make-text-edit (node-start node) (1+ (node-start node)) "")
                  (make-text-edit open-at open-at "("))
            :form-delta (if (top-level-node-p node) 1 0)))))

(defun transpose-nodes (parsed node)
  "Swap NODE with its next sibling."
  (ensure-editable node :transpose)
  (ensure-sibling-context node :transpose)
  (let ((next (next-sibling node)))
    (unless next
      (error 'edit-not-applicable :operation :transpose :node node :reason "no next sibling"))
    (ensure-editable next :transpose)
    (plan parsed :transpose
          (list (make-text-edit (node-start node) (node-end node) (node-text next))
                (make-text-edit (node-start next) (node-end next) (node-text node))))))

(defun node-ancestor-p (ancestor node)
  (loop for p = (node-parent node) then (node-parent p)
        while p
        thereis (eq p ancestor)))

(defun move-node (parsed node target-parsed target &key (where :after))
  "Move NODE next to TARGET (WHERE is :AFTER or :BEFORE), which may be in
another source. Returns a list of plans: one when both are in PARSED, else
the deletion from PARSED and the insertion into TARGET-PARSED. Verify every
plan before committing any."
  (ensure-editable node :move)
  (ensure-sibling-context node :move)
  (ensure-editable target :move)
  (ensure-sibling-context target :move)
  (let ((same (eq parsed target-parsed)))
    (when (eq node target)
      (error 'edit-not-applicable :operation :move :node node :reason "target is the form itself"))
    (when (and same (node-ancestor-p node target))
      (error 'edit-not-applicable :operation :move :node node :reason "target is inside the form"))
    (let* ((text (dedent-continuation (node-text node) (node-column node)))
           (removal (delete-node parsed node))
           (insertion (ecase where
                        (:after (insert-after target-parsed target text))
                        (:before (insert-before target-parsed target text)))))
      (if same
          (list (plan parsed :move
                      (append (edit-plan-edits removal) (edit-plan-edits insertion))
                      :form-delta (+ (edit-plan-form-delta removal) (edit-plan-form-delta insertion))))
          (list (plan parsed :move (edit-plan-edits removal)
                      :form-delta (edit-plan-form-delta removal))
                (plan target-parsed :move (edit-plan-edits insertion)
                      :form-delta (edit-plan-form-delta insertion)))))))

;;; Symbol-level operations

(defun token-name-span (node)
  "Offsets of the name part of a symbol or designator node's text."
  (let* ((s (node-string node))
         (start (node-start node))
         (end (node-end node)))
    (case (node-kind node)
      (:string (values (1+ start) (1- end)))
      (t (let ((colon (position #\: s :start start :end end :from-end t)))
           (values (if colon (1+ colon) (if (and (< (1+ start) end) (char= (char s start) #\#))
                                             (+ start 2) start))
                   end))))))

(defun styled-name (new-name old-text)
  "NEW-NAME in the case style of OLD-TEXT (lower, upper or as given)."
  (cond ((and (some #'alpha-char-p old-text) (every (lambda (c) (not (upper-case-p c))) old-text))
         (string-downcase new-name))
        ((and (some #'alpha-char-p old-text) (every (lambda (c) (not (lower-case-p c))) old-text))
         (string-upcase new-name))
        (t new-name)))

(defun rename-symbol (model package-name symbol-name new-name &key (sources (package-model-sources model)))
  "One EDIT-PLAN per source that mentions PACKAGE-NAME::SYMBOL-NAME, renaming
each mention (and DEFPACKAGE designators) to NEW-NAME. Package prefixes and
the original case style are kept."
  (read-edit-input new-name :min 1 :max 1)
  (let ((by-source (make-hash-table :test #'eq)))
    (dolist (ref (find-references model package-name symbol-name :sources sources))
      (push (cdr ref) (gethash (car ref) by-source)))
    (loop for ps in sources
          for nodes = (gethash ps by-source)
          when nodes
            collect (plan ps :rename
                          (mapcar (lambda (n)
                                    (multiple-value-bind (a b) (token-name-span n)
                                      (make-text-edit a b (styled-name new-name
                                                                       (subseq (node-string n) a b)))))
                                  nodes)))))

(defun add-export (parsed package-name symbol-name)
  "Add SYMBOL-NAME to the :EXPORT option of PACKAGE-NAME's DEFPACKAGE in PARSED."
  (let* ((model (build-package-model (list parsed)))
         (info (find-package-info model package-name)))
    (unless (and info (eq (package-info-source info) parsed))
      (error 'locator-not-found :locator (list :package package-name)))
    (when (member (string-upcase symbol-name) (package-info-exports info) :test #'string=)
      (return-from add-export (plan parsed :add-export '())))
    (let* ((form (package-info-node info))
           (options (cddr (node-forms form)))
           (exports (remove-if-not (lambda (o) (equal (option-name o) "EXPORT")) options))
           (option (car (last exports))))
      (if option
          (let* ((elements (rest (node-forms option)))
                 (last-el (car (last elements)))
                 (style (if last-el (node-text last-el) "#:x"))
                 (prefix (cond ((alexandria:starts-with-subseq "#:" style) "#:")
                               ((alexandria:starts-with-subseq ":" style) ":")
                               ((alexandria:starts-with-subseq "\"" style) "\"")
                               (t "")))
                 (name (if (string= prefix "\"")
                           (format nil "\"~A\"" (string-upcase symbol-name))
                           (concatenate 'string prefix (styled-name symbol-name (or (and last-el (node-text last-el)) "x"))))))
            (plan parsed :add-export
                  (list (if last-el
                            (make-text-edit (node-end last-el) (node-end last-el)
                                            (concatenate 'string (separator-for last-el) name))
                            (make-text-edit (1- (node-end option)) (1- (node-end option))
                                            (concatenate 'string " " name))))))
          (let ((anchor (car (last options))))
            (plan parsed :add-export
                  (list (make-text-edit (node-end (or anchor (second (node-forms form))))
                                        (node-end (or anchor (second (node-forms form))))
                                        (format nil "~%  (:export #:~A)"
                                                (string-downcase symbol-name))))))))))

;;; Repair

(defun repair-parens (parsed &key (imbalance (first (parsed-source-imbalances parsed))))
  "Plan applying IMBALANCE's suggested fix: insert missing closers or delete
the early / stray one. VERIFY-PLAN accepts it only if problems decrease."
  (unless imbalance
    (error 'edit-not-applicable :operation :repair-parens :node (parsed-source-root parsed)
                                :reason "no imbalance"))
  (let ((fix (imbalance-fix-position imbalance)))
    (unless fix
      (error 'edit-not-applicable :operation :repair-parens :node (parsed-source-root parsed)
                                  :reason "no suggested position"))
    (plan parsed :repair-parens
          (ecase (imbalance-kind imbalance)
            (:missing-close
             (list (make-text-edit fix fix (make-string (imbalance-count imbalance)
                                                        :initial-element #\)))))
            (:extra-close
             (list (make-text-edit fix (1+ fix) "")))
            (:unterminated-string
             (list (make-text-edit fix fix "\"")))
            (:unterminated-block-comment
             (list (make-text-edit fix fix (format nil "~%|#")))))
          :repair t)))
