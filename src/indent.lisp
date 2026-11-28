(in-package #:lisp-source-protocol)

;;; Table-driven Lisp indentation for code an edit inserts or replaces.
;;;
;;; The rules follow the usual Emacs/SLIME style:
;;;   - an operator with N distinguished arguments (from the table) indents
;;;     those by 4 and the body by 2 when they start a line;
;;;   - a function call aligns its arguments with the first argument when that
;;;     argument is on the operator's line, else one column past the paren;
;;;   - data (quoted lists, vectors, a list whose head is not a symbol) aligns
;;;     with the first element;
;;;   - a keyword head in code, (:export #:a #:b), aligns a line with the first
;;;     argument, or with the head when the line starts with a keyword (plist).
;;; Lines that start inside a string or block comment are left untouched.

(defvar *indentation* (make-hash-table :test #'equal)
  "Symbol name (upcased, no package) → indentation spec: an integer N of
distinguished arguments, :LOCAL-DEFINITIONS for FLET-like operators,
:PRESERVE to keep the input's relative indentation (LOOP),
(:CLAUSES N) for N distinguished arguments followed by clauses whose own
bodies indent like a one-argument operator (HANDLER-CASE), or (:REST N) for
arguments N columns in from the paren, aligned with the first one instead
when it is on the operator's line (COND; Emacs (&rest (&whole N …))).")

(defun set-indentation (name spec)
  "Record SPEC for operator NAME. SPEC is an integer, :LOCAL-DEFINITIONS,
:PRESERVE, (:CLAUSES N), (:REST N), (:SPECIAL N COLUMN) (N distinguished
arguments COLUMN in instead of 4, 0.2.8), NIL (remove), or a Swank/Emacs list
spec such as (4 4 &BODY) or (&LAMBDA &BODY): the number of elements before
&BODY is the distinguished-argument count, and (5 &BODY) is (:SPECIAL 1 5)."
  (let ((key (string-upcase name))
        (value (cond ((or (integerp spec) (member spec '(:local-definitions :bindings :preserve)) (null spec)
                          (and (consp spec) (member (first spec) '(:clauses :rest :special))))
                      spec)
                     ((listp spec)
                      (let ((body (position-if (lambda (x) (and (symbolp x) (string-equal x "&BODY")))
                                               spec)))
                        (if (and (eql body 1) (integerp (first spec)) (> (first spec) 4))
                            (list :special 1 (first spec))
                            body)))
                     (t nil))))
    (if value
        (setf (gethash key *indentation*) value)
        (remhash key *indentation*))
    value))

(defparameter +standard-indentation+
  '(("BLOCK" . 1) ("CASE" . 1) ("CCASE" . 1) ("ECASE" . 1) ("TYPECASE" . 1)
    ("CTYPECASE" . 1) ("ETYPECASE" . 1) ("CATCH" . 1) ("COND" :rest 2) ("DEFCONSTANT" . 1)
    ("DEFPARAMETER" . 1) ("DEFVAR" . 1) ("DEFPACKAGE" . 1) ("DEFINE-PACKAGE" . 1)
    ("DEFSTRUCT" . 1) ("DESTRUCTURING-BIND" . 2) ("DO" . 2) ("DO*" . 2)
    ("DOLIST" . 1) ("DOTIMES" . 1) ("EVAL-WHEN" . 1) ("FLET" . :local-definitions)
    ("LABELS" . :local-definitions) ("MACROLET" . :local-definitions)
    ("HANDLER-BIND" . :bindings) ("HANDLER-CASE" :clauses 1) ("RESTART-BIND" . :bindings)
    ("RESTART-CASE" :clauses 1) ("WHEN-LET" . 1) ("WHEN-LET*" . 1) ("IF-LET" . 1)
    ("IF-LET*" . 1) ("SWITCH" . 1) ("ESWITCH" . 1) ("CSWITCH" . 1) ("DESTRUCTURING-CASE" :clauses 1) ("WITH-SLOTS" . 2)
    ("WITH-ACCESSORS" . 2)
    ("IGNORE-ERRORS" . 0) ("LAMBDA" . 1) ("LET" . :bindings) ("LET*" . :bindings) ("LOCALLY" . 0) ("LOOP" . :preserve)
    ("MULTIPLE-VALUE-BIND" . 2) ("MULTIPLE-VALUE-PROG1" . 1) ("PROG1" . 1) ("PROG2" . 2)
    ("PROGN" . 0) ("PROGV" . 2) ("RETURN-FROM" . 1) ("SYMBOL-MACROLET" . :bindings)
    ;; 0.2.8 (#17): SLIME's (5 &body): the protected form 5 in
    ("TAGBODY" . 0) ("UNLESS" . 1) ("UNWIND-PROTECT" :special 1 5) ("WHEN" . 1)
    ("PRINT-UNREADABLE-OBJECT" . 1) ("DEFTEST" . 1) ("TESTING" . 1)
    ("DEFSYSTEM" . 1) ("DEFINE-CONDITION" . 2) ("DEFCLASS" . 2) ("DEFGENERIC" . 2)
    ("DEFMETHOD" . 2) ("DEFUN" . 2) ("DEFMACRO" . 2) ("DEFTYPE" . 2)
    ("DEFINE-COMPILER-MACRO" . 2) ("DEFSETF" . 2) ("DEFINE-SETF-EXPANDER" . 2)
    ("DEFINE-MODIFY-MACRO" . 2) ("DEFINE-METHOD-COMBINATION" . 2)
    ;; alexandria
    ("NAMED-LAMBDA" . 2)
    ;; CFFI: one distinguished argument (the name) unless noted
    ("DEFCSTRUCT" . 1) ("DEFCUNION" . 1) ("DEFCENUM" . 1) ("DEFBITFIELD" . 1)
    ("DEFCTYPE" . 1) ("DEFCVAR" . 1) ("DEFINE-FOREIGN-LIBRARY" . 1)
    ("DEFCFUN" . 2) ("DEFCALLBACK" . 3)))

(defparameter +lambda-list-positions+
  '(("DEFUN" . 2) ("DEFMACRO" . 2) ("DEFINE-COMPILER-MACRO" . 2) ("DEFGENERIC" . 2)
    ("DEFTYPE" . 2) ("DEFINE-SETF-EXPANDER" . 2) ("LAMBDA" . 1) ("NAMED-LAMBDA" . 2)
    ("DESTRUCTURING-BIND" . 1) ("MULTIPLE-VALUE-BIND" . 1))
  "0.2.8 (#17): operator name → the element index of its lambda list (or
variable list), which indents as SLIME's &lambda. DEFMETHOD's (and a
DEFGENERIC (:method …) option's) is its first list argument after the name,
FLET-like definitions' the element after the name.")

(defun reset-indentation ()
  "Restore *INDENTATION* to the standard table."
  (clrhash *indentation*)
  (loop for (name . spec) in +standard-indentation+
        do (setf (gethash name *indentation*) spec))
  *indentation*)

(reset-indentation)

(defparameter *method-option-operators* '("DEFGENERIC")
  "Operators whose (:method …) options indent like a DEFMETHOD body.")

(defun bare-operator-name (name)
  "NAME upcased, without a package prefix."
  (let* ((name (string-upcase name))
         (colon (position #\: name :from-end t)))
    (if colon (subseq name (1+ colon)) name)))

(defun operator-indentation (name)
  "Spec for operator NAME (any case, optional package prefix), or NIL for a
function call. Unknown DEF* / DEFINE-* operators take 2 distinguished
arguments, WITH-* and DO-* take 1."
  (let* ((name (string-upcase name))
         (colon (position #\: name :from-end t))
         (bare (if colon (subseq name (1+ colon)) name)))
    (multiple-value-bind (spec found) (gethash name *indentation*)
      (if found
          spec
          (multiple-value-bind (spec found) (gethash bare *indentation*)
            (cond (found spec)
                  ((and (> (length bare) 3) (string= "DEF" bare :end2 3)) 2)
                  ((and (> (length bare) 5) (string= "WITH-" bare :end2 5)) 1)
                  ((and (> (length bare) 3) (string= "DO-" bare :end2 3)) 1)
                  (t nil)))))))

;;; The scanner

(defstruct (indent-frame (:conc-name frame-))
  open-col open-line
  orig-open-col                         ; column of the paren in the input
  (kind :code)                          ; :code, :data, :local-definitions, :bindings or :lambda-list
  forced-spec                           ; spec imposed by the parent (FLET definitions)
  head                                  ; operator name, when the head is a symbol token
  (head-kind nil)                       ; :symbol, :keyword or :other
  (elements '())                        ; (col line keyword-p lambda-keyword-p), newest first
  (lambda-list-seen nil)                ; DEFMETHOD / (:method …): its lambda list opened
  (pending 0))                          ; prefix forms still to absorb (' #' #+)

(defun frame-element-count (frame) (length (frame-elements frame)))

(defun frame-element (frame index)
  "Element INDEX (0 = head) as (col line keyword-p), or NIL."
  (let ((count (frame-element-count frame)))
    (and (< index count) (nth (- count index 1) (frame-elements frame)))))

(defun frame-spec (frame)
  (or (frame-forced-spec frame)
      (and (eq (frame-head-kind frame) :symbol) (operator-indentation (frame-head frame)))))

(defun lambda-list-line-indent (frame lambda-keyword-line)
  "0.2.8 (#17): SLIME's default &lambda for a line in lambda-list FRAME (with
elements). A line starting with a lambda-list keyword aligns one past the
paren; any other line 2 past the nearest lambda-list keyword before it, or,
with none, with the first parameter."
  (if lambda-keyword-line
      (1+ (frame-open-col frame))
      (let ((keyword (find-if #'fourth (frame-elements frame))))
        (if keyword
            (+ (car keyword) 2)
            (car (frame-element frame 0))))))

(defun lambda-list-frame-p (parent parent-index grandparent)
  "True when a list opened as element PARENT-INDEX of PARENT (whose parent is
GRANDPARENT) is a lambda list (+LAMBDA-LIST-POSITIONS+, DEFMETHOD, a
(:method …) option, an FLET-like definition)."
  (and parent parent-index
       (eq (frame-kind parent) :code)
       (let ((head (and (frame-head parent) (bare-operator-name (frame-head parent)))))
         (cond ((null head) nil)
               ((eq (frame-head-kind parent) :symbol)
                (let ((position (cdr (assoc head +lambda-list-positions+ :test #'string=))))
                  (cond (position (eql parent-index position))
                        ((and (string= head "DEFMETHOD") (>= parent-index 2))
                         (not (frame-lambda-list-seen parent)))
                        ((and grandparent (eq (frame-kind grandparent) :local-definitions))
                         (eql parent-index 1)))))
               ((and (eq (frame-head-kind parent) :keyword) (string-equal head "METHOD")
                     (eql (frame-forced-spec parent) 1))
                (and (>= parent-index 1) (not (frame-lambda-list-seen parent))))))))

(defun line-indent-for (stack base &key old-indent keyword-line lambda-keyword-line)
  "Indentation column for a line whose first element goes into the top frame.
OLD-INDENT is the line's indentation in the input; KEYWORD-LINE is true when
the line starts with a keyword, LAMBDA-KEYWORD-LINE when it starts with a
lambda-list keyword (&key …)."
  (let ((frame (first stack)))
    (if (null frame)
        base
        (let ((n (frame-element-count frame))
              (open (frame-open-col frame)))
          (flet ((first-arg-on-open-line ()
                   (let ((first-arg (frame-element frame 1)))
                     (and first-arg (= (second first-arg) (frame-open-line frame)) first-arg))))
            (cond ((zerop n) (1+ open))
                  ;; 0.2.8 (#17): lambda lists as SLIME's &lambda
                  ((eq (frame-kind frame) :lambda-list)
                   (lambda-list-line-indent frame lambda-keyword-line))
                  ;; Keyword head in code, (:export #:a #:b): a line that starts
                  ;; with a keyword aligns with the head (plist style), any other
                  ;; line with the first argument when it is on the head's line.
                  ((and (eq (frame-head-kind frame) :keyword)
                        (eq (frame-kind frame) :code)
                        (not (frame-forced-spec frame))
                        (not keyword-line))
                   (let ((first-arg (first-arg-on-open-line)))
                     (if first-arg
                         (car first-arg)
                         (car (frame-element frame 0)))))
                  ((or (member (frame-kind frame) '(:data :local-definitions :bindings))
                       (and (not (eq (frame-head-kind frame) :symbol))
                            (not (frame-forced-spec frame))))
                   (car (frame-element frame 0)))
                  (t
                   (let ((spec (frame-spec frame)))
                     (cond ((and (consp spec) (eq (first spec) :rest))
                            (let ((first-arg (first-arg-on-open-line)))
                              (if first-arg (car first-arg) (+ open (second spec)))))
                           ;; (:special N COLUMN): N distinguished arguments COLUMN in
                           ((and (consp spec) (eq (first spec) :special))
                            (if (<= n (second spec)) (+ open (third spec)) (+ open 2)))
                           (t
                            (when (consp spec) (setf spec (second spec)))
                            (cond ((and (eq spec :preserve) old-indent)
                                   (max (1+ open) (+ old-indent (- open (frame-orig-open-col frame)))))
                                  ((eq spec :preserve)
                                   (let ((first-arg (frame-element frame 1)))
                                     (if first-arg (car first-arg) (1+ open))))
                                  ((integerp spec) (if (<= n spec) (+ open 4) (+ open 2)))
                                  ((member spec '(:local-definitions :bindings)) (if (<= n 1) (+ open 4) (+ open 2)))
                                  (t
                                   (let* ((first-arg (frame-element frame 1))
                                          (open-line (frame-open-line frame))
                                          (keyword-arg
                                            (and keyword-line first-arg (not (third first-arg))
                                                 ;; keyword/value pairs on the operator's line
                                                 (loop for k from 2 below n
                                                       for e = (frame-element frame k)
                                                       when (and (third e) (= (second e) open-line))
                                                         return (and (evenp (loop for j from k below n
                                                                                  count (= (second (frame-element frame j))
                                                                                           open-line)))
                                                                     e)))))
                                     (cond (keyword-arg (car keyword-arg))
                                           ((and first-arg (= (second first-arg) open-line))
                                            (car first-arg))
                                           (t (1+ open))))))))))))))))

(defun token-char-p (c)
  (not (member c '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\; #\' #\` #\,))))

(defun scan-indent-line (line line-number col0 state &optional (shift 0))
  "Scan LINE (output columns start at COL0; SHIFT = output column minus input
column on this line), updating STATE in place.
STATE is a list (stack in-string block-depth)."
  (destructuring-bind (stack in-string block-depth) state
    (let ((i 0) (n (length line)))
      (labels ((col () (+ col0 i))
               (start-element (&key (prefix 0))
                 "Register an element starting at the current column; returns the
frame it belongs to and whether it counts (not absorbed by a prefix)."
                 (let ((frame (first stack)))
                   (cond ((null frame) (values nil nil))
                         ((plusp (frame-pending frame))
                          (decf (frame-pending frame))
                          (incf (frame-pending frame) prefix)
                          (values frame nil))
                         (t (push (list (col) line-number (char= (char line i) #\:) (char= (char line i) #\&))
                                  (frame-elements frame))
                            (setf (frame-pending frame) prefix)
                            (values frame t)))))
               (push-frame (kind &optional (counted t))
                 (let* ((parent (first stack))
                        (parent-index (and parent (1- (frame-element-count parent))))
                        (parent-kind (and parent (frame-kind parent)))
                        (parent-spec (and parent (eq parent-kind :code) (frame-spec parent)))
                        ;; 0.2.8 (#17): a lambda list, or a list inside one
                        (lambda-list (and (not (eq kind :data)) counted
                                          (or (eq parent-kind :lambda-list)
                                              (lambda-list-frame-p parent parent-index (second stack))))))
                   (when (and lambda-list parent (not (eq parent-kind :lambda-list)))
                     (setf (frame-lambda-list-seen parent) t))
                   (push (make-indent-frame
                          :open-col (col) :orig-open-col (- (col) shift) :open-line line-number
                          :kind (cond ((eq kind :data) :data)
                                      (lambda-list :lambda-list)
                                      ((and counted (member parent-spec '(:local-definitions :bindings))
                                            (eql parent-index 1))
                                       parent-spec)
                                      (t :code))
                          ;; a local function's body indents like a DEFUN's, a
                          ;; binding's value 2 in unless it starts on the binding's
                          ;; line (SLIME's let), a clause like a one-argument
                          ;; operator
                          :forced-spec (cond (lambda-list nil)
                                             ((eq parent-kind :local-definitions) 1)
                                             ((eq parent-kind :bindings) '(:rest 2))
                                             ((and (consp parent-spec) (eq (first parent-spec) :clauses)
                                                   parent-index (> parent-index (second parent-spec)))
                                              1)))
                         stack)))
               (read-token ()
                 (let ((start i))
                   (loop while (< i n)
                         do (let ((c (char line i)))
                              (cond ((char= c #\\) (incf i 2))
                                    ((char= c #\|)
                                     (incf i)
                                     (loop while (and (< i n) (char/= (char line i) #\|))
                                           do (when (char= (char line i) #\\) (incf i))
                                              (incf i))
                                     (incf i))
                                    ((token-char-p c) (incf i))
                                    (t (return)))))
                   (subseq line start (min i n))))
               (note-head (frame counted text)
                 (when (and frame counted (= 1 (frame-element-count frame)))
                   (setf (frame-head frame) text
                         (frame-head-kind frame)
                         (cond ((and (plusp (length text)) (char= (char text 0) #\:)) :keyword)
                               ((or (zerop (length text)) (digit-char-p (char text 0))
                                    (char= (char text 0) #\#))
                                :other)
                               (t :symbol)))
                   ;; (:method lambda-list . body) inside DEFGENERIC indents like
                   ;; DEFMETHOD's tail: one distinguished argument, then the body
                   (let ((parent (second stack)))
                     (when (and (eq (frame-head-kind frame) :keyword)
                                (string-equal text ":method")
                                parent (eq (frame-head-kind parent) :symbol)
                                (member (bare-operator-name (frame-head parent)) *method-option-operators*
                                        :test #'string=))
                       (setf (frame-kind frame) :code
                             (frame-forced-spec frame) 1))))))
        (loop while (< i n)
              do (cond
                   ;; continuing a string
                   (in-string
                    (let ((c (char line i)))
                      (incf i)
                      (cond ((char= c #\\) (incf i))
                            ((char= c #\") (setf in-string nil)))))
                   ;; continuing a block comment
                   ((plusp block-depth)
                    (cond ((and (char= (char line i) #\|) (< (1+ i) n) (char= (char line (1+ i)) #\#))
                           (decf block-depth) (incf i 2))
                          ((and (char= (char line i) #\#) (< (1+ i) n) (char= (char line (1+ i)) #\|))
                           (incf block-depth) (incf i 2))
                          (t (incf i))))
                   (t
                    (let ((c (char line i)))
                      (case c
                        ((#\Space #\Tab #\Return #\Page) (incf i))
                        (#\; (setf i n))
                        (#\( (push-frame :code (nth-value 1 (start-element))) (incf i))
                        (#\) (when stack (pop stack)) (incf i))
                        (#\" (start-element) (setf in-string t) (incf i))
                        ((#\' #\`) (start-element :prefix 1) (incf i)
                         (when (and (char= c #\') (< i n) (char= (char line i) #\())
                           (start-element) (push-frame :data) (incf i)))
                        (#\, (start-element :prefix 1) (incf i)
                         (when (and (< i n) (member (char line i) '(#\@ #\.))) (incf i)))
                        (#\#
                         (let ((d (and (< (1+ i) n) (char line (1+ i)))))
                           (case d
                             (#\| (incf block-depth) (incf i 2))
                             (#\( (start-element) (incf i) (push-frame :data) (incf i))
                             (#\' (start-element :prefix 1) (incf i 2))
                             ((#\+ #\-) (start-element :prefix 2) (incf i 2))
                             (#\\ (multiple-value-bind (frame counted) (start-element)
                                    (incf i 3)
                                    (loop while (and (< i n) (token-char-p (char line i))) do (incf i))
                                    (note-head frame counted "#\\")))
                             (t
                              ;; #p"..", #S(..), #A(..), #C(..): a prefix; #:foo, #x1F: a token
                              (let ((j (1+ i)))
                                (loop while (and (< j n) (digit-char-p (char line j))) do (incf j))
                                (if (and (< (1+ j) n) (alpha-char-p (char line j))
                                         (member (char line (1+ j)) '(#\( #\")))
                                    (progn (start-element :prefix 1) (setf i (1+ j)))
                                    (multiple-value-bind (frame counted) (start-element)
                                      (note-head frame counted (read-token)))))))))
                        (t (multiple-value-bind (frame counted) (start-element)
                             (note-head frame counted (read-token))))))))))
      (setf (first state) stack
            (second state) in-string
            (third state) block-depth)
      state)))

(defun indent-state (string start start-column offset &key (state (list '() nil 0)) (first-line 0))
  "The indenter state at OFFSET of STRING, scanned from START (at
START-COLUMN) on from STATE (default: outside any list): the lists open at
OFFSET and what they hold so far. FIRST-LINE is the line number of START.
Second value: the line number of OFFSET. Pass both to INDENT-CODE as
:CONTEXT and :FIRST-LINE."
  (let ((line first-line))
    (loop with pos = start
          for col0 = start-column then 0
          for eol = (or (position #\Newline string :start pos :end offset) offset)
          do (scan-indent-line (subseq string pos eol) line col0 state)
          while (< eol offset)
          do (setf pos (1+ eol))
             (incf line))
    (values state line)))

(defun next-line-column (state base &key outer sibling-column old-indent keyword-line lambda-keyword-line)
  "Indentation of a line that starts at the end of the indenter's STATE.
BASE is the column of a top-level line. When the innermost open list is
OUTER (the context stack INDENT-CODE started from), the line starts an
element next to the code around it and gets SIBLING-COLUMN when given.
KEYWORD-LINE / LAMBDA-KEYWORD-LINE: the line starts with a keyword / a
lambda-list keyword (&key …)."
  (if (and sibling-column outer (eq (first state) outer))
      sibling-column
      (line-indent-for (first state) base :old-indent old-indent :keyword-line keyword-line
                                          :lambda-keyword-line lambda-keyword-line)))

(defun indent-code (text &key (column 0) (input-column column) context (first-line 0) sibling-column)
  "TEXT with every line after the first re-indented, as if TEXT started at
COLUMN. INPUT-COLUMN is where TEXT's first line was in its old place (used to
keep LOOP clauses' relative layout). CONTEXT is the indenter state where TEXT
goes, from INDENT-STATE, and FIRST-LINE the line number it was scanned to: a
form then indents as part of the lists around it (a local function, a
clause). A line that starts an element of the innermost of those lists gets
SIBLING-COLUMN when given. Lines that begin inside a string or block comment
keep their whitespace; blank lines lose trailing blanks.
Second value: the indenter state after TEXT, for NEXT-LINE-COLUMN."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (state (or context (list '() nil 0)))
         ;; with a context, a line outside every list is a top-level one
         (base (if context 0 column))
         (outer (first state)))
    (values
     (with-output-to-string (out)
       (loop for line in lines
             for index from 0
             for number from first-line
             do (let ((verbatim (or (zerop index) (second state) (plusp (third state)))))
                  (if verbatim
                      (progn
                        (unless (zerop index) (terpri out))
                        (write-string line out)
                        (scan-indent-line line number (if (zerop index) column 0) state
                                          (if (zerop index) (- column input-column) 0)))
                      (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
                             (old-indent (- (length line) (length trimmed)))
                             (blank (zerop (length (string-right-trim '(#\Space #\Tab #\Return) trimmed))))
                             (indent (if blank
                                         0
                                         (next-line-column state base
                                                           :outer outer :sibling-column sibling-column
                                                           :old-indent old-indent
                                                           :keyword-line (char= (char trimmed 0) #\:)
                                                           :lambda-keyword-line (char= (char trimmed 0) #\&)))))
                        (terpri out)
                        (unless blank
                          (write-string (make-string indent :initial-element #\Space) out)
                          (write-string trimmed out)
                          (scan-indent-line trimmed number indent state (- indent old-indent))))))))
     state)))

(defun shift-continuation (text delta)
  "TEXT with every line after the first moved DELTA columns (left by at most
its leading blanks), except lines that begin inside a string or block
comment: their text is part of the string or comment."
  (if (zerop delta)
      text
      (let ((state (list '() nil 0)))
        (with-output-to-string (out)
          (loop for line in (uiop:split-string text :separator '(#\Newline))
                for index from 0
                do (unless (zerop index) (terpri out))
                   (let ((blanks (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line)
                                     (length line))))
                     (cond ((or (zerop index) (second state) (plusp (third state)) (= blanks (length line)))
                            (write-string line out))
                           ((plusp delta)
                            (write-string (make-string delta :initial-element #\Space) out)
                            (write-string line out))
                           (t (write-string line out :start (min blanks (- delta))))))
                   (scan-indent-line line index 0 state))))))

(defun dedent-continuation (text column)
  "Remove up to COLUMN leading blanks from every line of TEXT after the first,
undoing the indentation a form had at COLUMN in its old place. Lines inside
strings and block comments are kept."
  (shift-continuation text (- column)))

(defun macro-lambda-list-indentation (lambda-list)
  "Distinguished-argument count for a DEFMACRO LAMBDA-LIST node: the positional
parameters before &BODY (&WHOLE and &ENVIRONMENT pairs skipped), or NIL when
there is no &BODY."
  (let ((count 0) (skip nil))
    (dolist (p (node-forms lambda-list) nil)
      (let ((name (node-symbol-name p)))
        (cond (skip (setf skip nil))
              ((and name (member name '("&WHOLE" "&ENVIRONMENT") :test #'string-equal))
               (setf skip t))
              ((and name (string-equal name "&BODY")) (return count))
              ((and name (char= (char name 0) #\&)) nil)
              (t (incf count)))))))

(defun learn-indentation (sources)
  "Record the indentation of every DEFMACRO with a &BODY parameter in SOURCES
(parsed sources), so code calling those macros indents like their definition
says. Returns the number of macros learned."
  (let ((learned 0))
    (labels ((walk (form)
               (let ((head (node-head-name form)))
                 (cond ((null head))
                       ((string-equal head "DEFMACRO")
                        (destructuring-bind (&optional op name lambda-list &rest body) (node-forms form)
                          (declare (ignore op body))
                          (let ((symbol (and name (node-symbol-name name))))
                            (when (and symbol lambda-list (eq (node-kind lambda-list) :list))
                              (let ((n (macro-lambda-list-indentation lambda-list)))
                                (when (and n (not (gethash (string-upcase symbol) *indentation*)))
                                  (set-indentation symbol n)
                                  (incf learned)))))))
                       ((member head '("PROGN" "EVAL-WHEN") :test #'string-equal)
                        (mapc #'walk (node-forms form)))))))
      (dolist (ps sources)
        (mapc #'walk (parsed-source-forms ps))))
    learned))
