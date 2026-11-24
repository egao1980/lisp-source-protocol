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

(defun read-edit-source (text &key (min 1) max)
  "Read TEXT as edit input and return its PARSED-SOURCE, comments included, or
signal EDIT-INPUT-UNREADABLE. MIN / MAX bound the number of forms."
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
    ps))

(defun read-edit-input (text &key (min 1) max)
  "Read TEXT as edit input. Return its top-level expression nodes, or signal
EDIT-INPUT-UNREADABLE. MIN / MAX bound the number of forms."
  (parsed-source-forms (read-edit-source text :min min :max max)))

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

(defun lay-out-code (text node offset &key (input-column 0) (shift 0) sibling-column)
  "TEXT as it goes at OFFSET, next to NODE: re-indented with the lists open
there as context (INDENT-CODE), so it indents as part of the code around it,
and lines that start an element of the innermost of those lists at
SIBLING-COLUMN when given. INPUT-COLUMN is where TEXT's first line started in
the edit input. With *REINDENT* NIL, the lines after the first only move
SHIFT columns. Second value: the indenter state after TEXT."
  (multiple-value-bind (context line) (context-at node offset)
    (let ((column (offset-column node offset)))
      (if *reindent*
          (indent-code text :column column :input-column input-column :context context
                            :first-line line :sibling-column sibling-column)
          (let ((moved (shift-continuation text shift)))
            (values moved
                    (indent-state moved 0 column (length moved) :state context :first-line line)))))))

(defun indent-continuation (text column)
  "Shift every line of TEXT after the first right by COLUMN spaces (not the
lines inside strings and block comments)."
  (shift-continuation text column))

(defun ws-char-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun skip-ws-forward (s pos)
  (or (position-if-not #'ws-char-p s :start pos) (length s)))

(defun skip-ws-backward (s pos)
  "Offset after the last non-whitespace character before POS."
  (let ((p (position-if-not #'ws-char-p s :end pos :from-end t)))
    (if p (1+ p) 0)))

(defun line-start (s pos)
  "Offset of the start of the line holding POS."
  (let ((nl (position #\Newline s :end pos :from-end t)))
    (if nl (1+ nl) 0)))

(defun comment-line-p (s from to)
  "True when S[FROM, TO) is blanks then a ; comment."
  (let ((first (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s :start from :end to)))
    (and first (char= (char s first) #\;))))

(defun leading-comment-start (node)
  "Where a top-level NODE begins once the ; comment lines directly above it
(no blank line in between) count as part of it; NODE-START otherwise. Delete,
transpose and move carry those comments with the form."
  (let* ((s (node-string node))
         (start (node-start node))
         (begin (line-start s start)))
    (when (or (not (top-level-node-p node))
              (find-if-not #'ws-char-p s :start begin :end start))
      (return-from leading-comment-start start))
    ;; BEGIN is the start of NODE's line; step back one line at a time
    (loop while (plusp begin)
          do (let* ((prev-end (1- begin))
                    (prev (line-start s prev-end)))
               (if (comment-line-p s prev prev-end)
                   (setf begin prev)
                   (return))))
    (if (< begin (line-start s start)) begin start)))

(defun sibling-forms (node)
  (node-forms (node-parent node)))

(defun next-sibling (node)
  (second (member node (sibling-forms node))))

(defun previous-sibling (node)
  (let ((forms (sibling-forms node)))
    (loop for (a b) on forms
          when (eq b node) do (return a))))

(defun conditional-unit (node)
  "NODE, or the outermost #+/#- conditional whose branch it is: inserting
next to, deleting, transposing or moving a branch's form acts on the whole
conditional, so its prefix never captures another form."
  (loop for parent = (node-parent node)
        while (and parent (eq (node-kind parent) :feature-conditional)
                   (member node (feature-conditional-forms parent)))
        do (setf node parent))
  node)

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

(defun blank-char-p (c) (member c '(#\Space #\Tab)))

(defun skip-blanks (s pos)
  "Offset of the first character at or after POS that is not a space or tab."
  (or (position-if-not #'blank-char-p s :start pos) (length s)))

(defun line-end (s pos)
  "Offset of the newline that ends the line holding POS, or the length of S."
  (or (position #\Newline s :start pos) (length s)))

(defun line-break-between-p (s a b)
  "True when a newline lies between offsets A and B of S."
  (and (< a b) (find #\Newline s :start a :end b) t))

(defun offset-column (node offset)
  "Column of OFFSET in NODE's source."
  (nth-value 1 (offset-line-column (node-source node) offset)))

(defun spaces (n) (make-string n :initial-element #\Space))

(defun top-level-ancestor (node)
  "The top-level form NODE is part of: NODE itself when it is one."
  (loop until (or (null (node-parent node)) (top-level-node-p node))
        do (setf node (node-parent node)))
  node)

(defun context-at (node offset)
  "The indenter state at OFFSET, inside NODE's top-level form or after it, and
the line of OFFSET counted from that form's first line (INDENT-STATE)."
  (let ((top (top-level-ancestor node)))
    (if (<= offset (node-start top))
        (values (list '() nil 0) 0)
        (indent-state (node-string node) (node-start top) (node-column top) offset))))

(defun line-tail (s pos)
  "What follows POS on its line: :BLANK; :COMMENT, a ; comment after blanks;
:CLOSERS, closing parens and then nothing or a ; comment; or :CODE. Second
value: the end of the closing parens; third: the start of the comment."
  (let ((p (skip-blanks s pos)))
    (flet ((line-done-p (i) (or (= i (length s)) (member (char s i) '(#\Newline #\Return)))))
      (cond ((line-done-p p) :blank)
            ((char= (char s p) #\;) (values :comment nil p))
            ((char= (char s p) #\))
             (let* ((q (or (position-if-not (lambda (c) (or (char= c #\)) (blank-char-p c))) s :start p)
                           (length s)))
                    (closers-end (1+ (position #\) s :end q :from-end t))))
               (cond ((line-done-p q) (values :closers closers-end nil))
                     ((char= (char s q) #\;) (values :closers closers-end q))
                     (t (values :code closers-end nil)))))
            (t :code)))))

(defun comment-end (node)
  "NODE's end, before the newline a ; comment ends with."
  (let ((end (node-end node)))
    (if (and (eq (node-kind node) :comment) (> end (node-start node))
             (char= (char (node-string node) (1- end)) #\Newline))
        (1- end)
        end)))

(defun comments-above (node)
  "Start of the ; comment lines directly above NODE in its list (no blank line
in between), or NODE-START. NODE starts its line."
  (let* ((s (node-string node))
         (siblings (node-children (node-parent node)))
         (start (node-start node)))
    (loop for c in (reverse (subseq siblings 0 (position node siblings)))
          while (and (eq (node-kind c) :comment) (line-prefix-blank-p c)
                     (= (node-end c) (line-start s start)))
          do (setf start (node-start c)))
    start))

(defstruct (input-layout (:conc-name input-))
  (text "" :type string)
  (comment nil)
  (gap "")
  (column 0)
  (ends-in-comment nil))

(defun trailing-input-comment (input)
  "The ; comment after the last form of edit input INPUT, on that form's line."
  (let* ((nodes (parsed-source-nodes input))
         (last (car (last nodes)))
         (before (car (last nodes 2))))
    (and (eq (node-kind last) :comment) (not (eq before last))
         (not (line-break-between-p (node-string last) (node-end before) (node-start last)))
         last)))

(defun input-multi-line-p (input)
  "True when edit input INPUT, comments included, spans more than one line."
  (let ((nodes (parsed-source-nodes input)))
    (line-break-between-p (node-string (first nodes)) (node-start (first nodes))
                          (comment-end (car (last nodes))))))

(defun lay-out-input (input &optional split)
  "Edit input INPUT (a PARSED-SOURCE) as written, comments included: its text
from the first form or comment to the last. SPLIT, a string, replaces the
blanks between two forms that share a line. A ; comment after the last form
is kept apart (COMMENT, after the blanks GAP), so it can follow closing
parens."
  (let* ((nodes (parsed-source-nodes input))
         (s (node-string (first nodes)))
         (comment (trailing-input-comment input))
         (body (if comment (butlast nodes) nodes))
         (start (node-start (first nodes)))
         (end (comment-end (car (last body))))
         (pos start))
    (make-input-layout
     :text (with-output-to-string (out)
             (when split
               (loop for (a b) on body
                     when (and b (expression-node-p a) (expression-node-p b)
                               (not (line-break-between-p s (node-end a) (node-start b))))
                       do (write-string s out :start pos :end (node-end a))
                          (write-string split out)
                          (setf pos (node-start b))))
             (write-string s out :start pos :end end))
     :comment (and comment (subseq s (node-start comment) (comment-end comment)))
     :gap (if comment (subseq s (node-end (car (last body))) (node-start comment)) "")
     :column (offset-column (first nodes) start)
     :ends-in-comment (eq (node-kind (car (last body))) :comment))))

(defun input-tail (layout)
  "LAYOUT's trailing comment with the blanks before it, or \"\"."
  (if (input-comment layout)
      (concatenate 'string (input-gap layout) (input-comment layout))
      ""))

;;; Operations. Each takes the PARSED-SOURCE the node belongs to.

(defun replace-node (parsed node new-text)
  "Replace NODE with exactly one form read from NEW-TEXT, with the comments
written around it. A comment before the form starts its own line; one after
it goes after the closing parens that follow NODE, or, when code follows on
the line, that code moves to the next line."
  (ensure-editable node :replace)
  (when (consing-dot-node-p node)
    (error 'edit-not-applicable :operation :replace :node node :reason "the consing dot"))
  (let* ((input (read-edit-source new-text :min 1 :max 1))
         (layout (lay-out-input input))
         (s (node-string node))
         (start (node-start node))
         (end (node-end node))
         (leading (not (expression-node-p (first (parsed-source-nodes input)))))
         (break-before (and leading (not (line-prefix-blank-p node))))
         (from (if break-before (skip-ws-backward s start) start))
         (sibling (and (line-prefix-blank-p node) (not (top-level-node-p node)) (node-column node))))
    (multiple-value-bind (text state)
        (lay-out-code (concatenate 'string (if break-before (format nil "~%~A" (spaces (input-column layout))) "")
                                   (input-text layout))
                      node from :input-column (input-column layout) :sibling-column sibling
                      :shift (- (node-column node) (input-column layout)))
      (multiple-value-bind (tail closers-end) (line-tail s end)
        (flet ((new-line (keyword)
                 (format nil "~%~A" (spaces (if (top-level-node-p node)
                                                0
                                                (next-line-column state 0 :keyword-line keyword))))))
          (plan parsed :replace
                (list
                 (case tail
                   (:closers
                    (let ((closers (remove-if #'blank-char-p (subseq s end closers-end))))
                      (if (or (input-comment layout) (input-ends-in-comment layout))
                          (make-text-edit from closers-end
                                          (concatenate 'string text
                                                       (if (input-ends-in-comment layout) (new-line nil) "")
                                                       closers (input-tail layout)))
                          (make-text-edit from end text))))
                   (:code
                    (let* ((after (skip-blanks s end))
                           (follow (find after (sibling-forms node) :key #'node-start)))
                      (cond ((or (input-comment layout) (input-ends-in-comment layout))
                             ;; what followed on the line starts the next one
                             (let ((line (new-line (char= (char s after) #\:))))
                               (make-text-edit from (if follow (node-end follow) after)
                                               (concatenate 'string text (input-tail layout) line
                                                            (if follow
                                                                (shift-continuation (node-text follow)
                                                                                    (- (length line) 1 (node-column follow)))
                                                                "")))))
                            (follow
                             ;; the next sibling on the line moves with the end of the new text
                             (let* ((last-nl (position #\Newline text :from-end t))
                                    (column (+ (if last-nl
                                                   (- (length text) last-nl 1)
                                                   (+ (offset-column node from) (length text)))
                                               (- after end))))
                               (make-text-edit from (node-end follow)
                                               (concatenate 'string text (subseq s end after)
                                                            (shift-continuation (node-text follow)
                                                                                (- column (node-column follow)))))))
                            (t (make-text-edit from end text)))))
                   (t (make-text-edit from end (concatenate 'string text (input-tail layout))))))))))))

(defun separator-for (node)
  "Whitespace placed between NODE and an inserted sibling."
  (cond ((top-level-node-p node) (format nil "~%~%"))
        ((line-prefix-blank-p node)
         (format nil "~%~A" (make-string (node-column node) :initial-element #\Space)))
        (t " ")))

(defun insert-after (parsed node text)
  "Insert the forms read from TEXT after NODE, as siblings, with the comments
written around them. Every form is written, #+/#- ones included whatever the
reading features. A comment that ends NODE's line stays on it."
  (let ((node (conditional-unit node)))
    (ensure-editable node :insert-after)
    (ensure-sibling-context node :insert-after)
    (let ((input (read-edit-source text)))
      (plan parsed :insert-after (list (insert-after-edit node input))
                   :form-delta (if (top-level-node-p node) (length (parsed-source-forms input)) 0)))))

(defun insert-before (parsed node text)
  "Insert the forms read from TEXT before NODE, as siblings, with the comments
written around them. Every form is written, #+/#- ones included whatever the
reading features. The ; comment lines right above NODE stay with it."
  (let ((node (conditional-unit node)))
    (ensure-editable node :insert-before)
    (ensure-sibling-context node :insert-before)
    (let ((input (read-edit-source text)))
      (plan parsed :insert-before (list (insert-before-edit node input))
                   :form-delta (if (top-level-node-p node) (length (parsed-source-forms input)) 0)))))

(defun insert-after-edit (node input)
  "Text edit putting edit input INPUT (a PARSED-SOURCE) after NODE. The new
code goes on a line of its own when NODE's next sibling is on a later line,
or when NODE is the last element and starts its line or the code spans
lines; after a top-level form, after a blank line. Otherwise it goes on
NODE's line. A comment that ends NODE's line stays there, and one after the
last new form goes after the closing parens that follow it."
  (let* ((s (node-string node))
         (top (top-level-node-p node))
         (end (node-end node))
         (next (unless top (next-sibling node)))
         (multi (input-multi-line-p input))
         (input-comment (trailing-input-comment input)))
    (multiple-value-bind (tail closers-end line-comment) (line-tail s end)
      (let* ((own (or top (eq tail :comment)
                      (if next
                          (line-break-between-p s end (node-start next))
                          (or (line-prefix-blank-p node) multi (and line-comment input-comment t)))))
             (lead (cond (top (format nil "~%~%")) (own (string #\Newline)) (t " ")))
             (layout (lay-out-input input (and own lead)))
             (body (concatenate 'string lead (if own (spaces (input-column layout)) "") (input-text layout)))
             (sibling (cond (top nil)
                            ((and next (line-prefix-blank-p next)) (node-column next))
                            ((line-prefix-blank-p node) (node-column node))))
             (ends-in-comment (or (input-comment layout) (input-ends-in-comment layout))))
        (flet ((lay-out (text at)
                 (lay-out-code text node at
                               :input-column (input-column layout) :sibling-column sibling
                               :shift (- (if own (or sibling (node-column node)) (1+ (offset-column node at)))
                                         (input-column layout)))))
          (ecase tail
            ((:blank :comment)
             (let ((at (if (eq tail :comment) (line-end s end) end)))
               (make-text-edit at at (concatenate 'string (lay-out body at) (input-tail layout)))))
            (:closers
             (let ((closers (remove-if #'blank-char-p (subseq s end closers-end))))
               (if own
                   ;; the closers move after the new code; NODE's comment stays
                   (let ((moved (if line-comment (subseq s closers-end (line-end s end)) "")))
                     (multiple-value-bind (text state) (lay-out (concatenate 'string moved body) end)
                       (make-text-edit end (if line-comment (line-end s end) closers-end)
                                       (concatenate 'string text
                                                    (if (input-ends-in-comment layout)
                                                        (format nil "~%~A" (spaces (next-line-column state 0)))
                                                        "")
                                                    closers (input-tail layout)))))
                   (let ((text (lay-out body end)))
                     (if (input-comment layout)
                         (make-text-edit end closers-end (concatenate 'string text closers (input-tail layout)))
                         (make-text-edit end end text))))))
            (:code
             (let* ((after (skip-blanks s end))
                    (follow (and next (= (node-start next) after) next))
                    (break (or top (if follow (or multi ends-in-comment) ends-in-comment))))
               (multiple-value-bind (text state) (lay-out body end)
                 (cond (break
                        ;; what followed on the line starts the next one
                        (let ((column (if top 0 (next-line-column state 0 :keyword-line (char= (char s after) #\:)))))
                          (make-text-edit end (if follow (node-end follow) after)
                                          (concatenate 'string text (input-tail layout)
                                                       (if top (format nil "~%~%") (format nil "~%~A" (spaces column)))
                                                       (if follow
                                                           (shift-continuation (node-text follow) (- column (node-column follow)))
                                                           "")))))
                       (follow
                        ;; the next sibling moves right on its line
                        (make-text-edit end (node-end follow)
                                        (concatenate 'string text (subseq s end after)
                                                     (shift-continuation (node-text follow) (length text)))))
                       (t (make-text-edit end end text))))))))))))

(defun insert-before-edit (node input)
  "Text edit putting edit input INPUT (a PARSED-SOURCE) before NODE. Before a
form that starts its line, the new code goes on lines of its own above it and
above the ; comment lines over it (a top-level form: then a blank line).
Otherwise it takes NODE's place, and NODE moves to the next line when its
next sibling is on a later line or the new code spans lines or ends with a
comment; else NODE stays on the line, after it."
  (let* ((s (node-string node))
         (start (node-start node))
         (next (unless (top-level-node-p node) (next-sibling node))))
    (cond ((top-level-node-p node)
           (let ((at (leading-comment-start node))
                 (layout (lay-out-input input (format nil "~%~%"))))
             (make-text-edit at at (concatenate 'string
                                                (lay-out-code (input-text layout) node at
                                                              :input-column (input-column layout)
                                                              :shift (- (input-column layout)))
                                                (input-tail layout) (format nil "~%~%")))))
          ((line-prefix-blank-p node)
           (let* ((at (comments-above node))
                  (column (offset-column node at))
                  (layout (lay-out-input input (string #\Newline))))
             (make-text-edit at at (concatenate 'string
                                                (lay-out-code (input-text layout) node at
                                                              :input-column (input-column layout)
                                                              :sibling-column (node-column node)
                                                              :shift (- column (input-column layout)))
                                                (input-tail layout)
                                                (format nil "~%~A" (spaces column))))))
          (t
           (let* ((down (or (input-multi-line-p input) (trailing-input-comment input)
                            (and next (line-break-between-p s (node-end node) (node-start next)))))
                  (layout (lay-out-input input (and down (string #\Newline))))
                  (sibling (and next (line-prefix-blank-p next) (node-column next))))
             (multiple-value-bind (text state)
                 (lay-out-code (input-text layout) node start
                               :input-column (input-column layout) :sibling-column sibling
                               :shift (- (node-column node) (input-column layout)))
               (let* ((head (concatenate 'string text (input-tail layout)))
                      (column (if down
                                  (or sibling (next-line-column state 0 :keyword-line (char= (char s start) #\:)))
                                  (+ (node-column node) (length head) 1))))
                 (make-text-edit start (node-end node)
                                 (concatenate 'string head
                                              (if down (format nil "~%~A" (spaces column)) " ")
                                              (shift-continuation (node-text node) (- column (node-column node))))))))))))

(defun delete-node (parsed node)
  "Delete NODE and the whitespace that separated it from its neighbours. The
form of a #+/#- branch goes with its prefix."
  (setf node (conditional-unit node))
  (ensure-editable node :delete)
  (ensure-sibling-context node :delete)
  (let* ((s (node-string node))
         (start (node-start node))
         (end (node-end node))
         (after (skip-ws-forward s end))
         (closer-next (and (< after (length s)) (char= (char s after) #\))))
         (prev (previous-sibling node)))
    (when (and (top-level-node-p node) prev (null (next-sibling node)))
      ;; the last top-level form: take the blank lines before it too, keep
      ;; the file's final newline
      (return-from delete-node
        (plan parsed :delete
              (list (make-text-edit (skip-ws-backward s (leading-comment-start node)) after
                                    (if (and (= after (length s)) (plusp after)
                                             (char= (char s (1- after)) #\Newline))
                                        (string #\Newline)
                                        "")))
              :form-delta -1)))
    (multiple-value-bind (del-start del-end)
        (cond ((top-level-node-p node) (values (leading-comment-start node) after))
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
                (multiple-value-bind (context line) (context-at node (node-start node))
                  (list (make-text-edit (node-start node) (node-end node)
                                        (indent-code (concatenate 'string open (node-text node) ")")
                                                     :column column :context context :first-line line
                                                     :input-column (if multi-line
                                                                       column
                                                                       (- column (length open))))))))
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
  "Move the closing paren of list NODE past its next sibling. A sibling on a
later line lands on its own line, at the column of NODE's elements."
  (ensure-editable node :slurp-forward)
  (ensure-sibling-context node :slurp-forward)
  (ensure-list-node node :slurp-forward)
  (let ((next (next-sibling node)))
    (unless next
      (error 'edit-not-applicable :operation :slurp-forward :node node :reason "no next sibling"))
    (plan parsed :slurp-forward (slurp-forward-edits node next)
                 :form-delta (if (top-level-node-p node) -1 0))))

(defun slurp-forward-edits (node next)
  "Text edits moving the closing paren of NODE past NEXT, its next sibling."
  (let* ((s (node-string node))
         (close (1- (node-end node)))
         (last (car (last (node-forms node))))
         (inner-end (if last (node-end last) (1+ (node-start node)))))
    (if (or (find-if-not #'ws-char-p s :start inner-end :end close)
            (find-if-not #'ws-char-p s :start (node-end node) :end (node-start next)))
        ;; a comment in the way: move the paren alone
        (list (make-text-edit close (node-end node) "")
              (make-text-edit (node-end next) (node-end next) ")"))
        (let* ((later (and last (line-break-between-p s (node-end node) (node-start next))))
               (gap (cond (later "") (last (subseq s (node-end node) (node-start next))) (t "")))
               (column (cond ((not later) (+ (offset-column node inner-end) (length gap)))
                             ((line-prefix-blank-p last) (node-column last))
                             (t (next-line-column (context-at node close) 0)))))
          (list (make-text-edit inner-end (node-end next)
                                (concatenate 'string (if later (format nil "~%~A" (spaces column)) gap)
                                             (shift-continuation (node-text next) (- column (node-column next)))
                                             ")")))))))

(defun slurp-backward (parsed node)
  "Move the opening paren of list NODE before its previous sibling. When
NODE's elements were on a later line than that sibling, they keep their own
lines, aligned under it."
  (ensure-editable node :slurp-backward)
  (ensure-sibling-context node :slurp-backward)
  (ensure-list-node node :slurp-backward)
  (let ((prev (previous-sibling node)))
    (unless prev
      (error 'edit-not-applicable :operation :slurp-backward :node node :reason "no previous sibling"))
    (plan parsed :slurp-backward (slurp-backward-edits node prev)
                 :form-delta (if (top-level-node-p node) -1 0))))

(defun slurp-backward-edits (node prev)
  "Text edits moving the opening paren of NODE before PREV, its previous
sibling."
  (let* ((s (node-string node))
         (open (node-start node))
         (first (first (node-forms node))))
    (if (or (null first)
            (find-if-not #'ws-char-p s :start (node-end prev) :end open)
            (find-if-not #'ws-char-p s :start (1+ open) :end (node-start first)))
        ;; empty, or a comment in the way: move the paren alone
        (list (make-text-edit (node-start prev) (node-start prev) "(")
              (make-text-edit open (1+ open) ""))
        (let* ((head (concatenate 'string "(" (shift-continuation (node-text prev) 1)))
               (later (line-break-between-p s (node-end prev) (node-start first)))
               (gap (if later "" (subseq s (node-end prev) open)))
               (last-nl (position #\Newline head :from-end t))
               (column (cond (later
                              (multiple-value-bind (context line) (context-at node (node-start prev))
                                (next-line-column (indent-state head 0 (node-column prev) (length head)
                                                                :state context :first-line line)
                                                  0)))
                             (last-nl (+ (- (length head) last-nl 1) (length gap)))
                             (t (+ (node-column prev) (length head) (length gap))))))
          (list (make-text-edit (node-start prev) (node-end node)
                                (concatenate 'string head (if later (format nil "~%~A" (spaces column)) gap)
                                             (shift-continuation (subseq s (node-start first) (node-end node))
                                                                 (- column (node-column first))))))))))

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
  (setf node (conditional-unit node))
  (ensure-editable node :transpose)
  (ensure-sibling-context node :transpose)
  (let ((next (next-sibling node)))
    (unless next
      (error 'edit-not-applicable :operation :transpose :node node :reason "no next sibling"))
    (ensure-editable next :transpose)
    (let ((s (node-string node))
          (a (leading-comment-start node))
          (b (leading-comment-start next)))
      (plan parsed :transpose
            (list (make-text-edit a (node-end node) (subseq s b (node-end next)))
                  (make-text-edit b (node-end next) (subseq s a (node-end node))))))))

(defun node-ancestor-p (ancestor node)
  (loop for p = (node-parent node) then (node-parent p)
        while p
        thereis (eq p ancestor)))

(defun move-node (parsed node target-parsed target &key (where :after))
  "Move NODE next to TARGET (WHERE is :AFTER or :BEFORE), which may be in
another source. Returns a list of plans: one when both are in PARSED, else
the deletion from PARSED and the insertion into TARGET-PARSED. Verify every
plan before committing any."
  (setf node (conditional-unit node)
        target (conditional-unit target))
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
           (insertion (if (and (top-level-node-p node) (top-level-node-p target))
                          ;; whole lines, leading comments included: the
                          ;; forms reader would drop the comments
                          (let ((chunk (subseq (node-string node) (leading-comment-start node) (node-end node))))
                            (read-edit-input text)
                            (plan target-parsed :insert
                                  (list (ecase where
                                          (:after (make-text-edit (node-end target) (node-end target)
                                                                  (format nil "~%~%~A" chunk)))
                                          (:before (let ((at (leading-comment-start target)))
                                                     (make-text-edit at at (format nil "~A~%~%" chunk))))))
                                  :form-delta 1))
                          (ecase where
                            (:after (insert-after target-parsed target text))
                            (:before (insert-before target-parsed target text))))))
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
