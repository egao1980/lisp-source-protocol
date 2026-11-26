(in-package #:lisp-source-protocol/tests)

;;; Comment blocks: consecutive ; / #| |# comment lines between forms,
;;; addressed by (:comment line [last-line]) or by (:line N) on a top-level
;;; comment, replaced and deleted with the same checks as forms.

(defparameter *comment-source*
  (lines ";;; Header line one"
         ";;; Header line two"
         ""
         "(defun f (x)"
         "  ;; inside f"
         "  ;; still inside"
         "  x)"
         ""
         ";;; --- 3. mail"
         ""
         "(defun g () 2)"
         ""))

(defun comment-op (source locator op &rest args)
  "Apply OP to the node LOCATOR finds in SOURCE; return the verified new text."
  (let* ((ps (read-source source))
         (plan (apply op ps (locate ps locator) args)))
    (source-string (parsed-source-text (verify-plan plan)))))

(deftest locate-comment-blocks
  (let ((ps (read-source *comment-source*)))
    (let ((block (locate ps '(:comment 1))))
      (ok (comment-block-p block))
      (ok (string= (node-text block) (lines ";;; Header line one" ";;; Header line two"))))
    (ok (string= (node-text (locate ps '(:comment 2))) ";;; Header line two")
        "the block starting on line 2")
    (ok (string= (node-text (locate ps '(:comment 1 1))) ";;; Header line one")
        "a last line bounds the block")
    (ok (string= (node-text (locate ps '(:comment 5)))
                 (lines ";; inside f" "  ;; still inside"))
        "a comment block inside a form")
    (ok (string= (node-text (locate ps '(:line 9 :column 0))) ";;; --- 3. mail")
        "line: on a top-level comment gives the comment block")
    (ok (string= (node-text (locate ps '(:line 1 :column 0)))
                 (lines ";;; Header line one" ";;; Header line two")))
    (ok (eql 0 (search "(defun f" (node-text (locate ps '(:line 5 :column 0)))))
        "line: inside a form still gives the innermost form")
    (ok (signals (locate ps '(:comment 4)) 'locator-not-found) "not a comment line")
    (ok (signals (locate ps '(:comment 3)) 'locator-not-found) "a blank line")))

(deftest comment-block-after-code-is-not-a-block
  (let ((ps (read-source (lines "(a) ; trailing" "#| x |# (b)" ""))))
    (ok (signals (locate ps '(:comment 1)) 'locator-not-found))
    (ok (signals (locate ps '(:comment 2)) 'locator-not-found))))

(deftest-parametrize comment-block-delete
    ((source locator expected)
     ;; between two forms: one blank line stays
     ((lines "(a)" "" ";;; old" "" "(b)" "") '(:comment 3) (lines "(a)" "" "(b)" ""))
     ;; at the start of the file: the blank lines after it go too
     ((lines ";;; one" ";;; two" "" "(a)" "") '(:comment 1) (lines "(a)" ""))
     ;; directly above a form: only the comment lines go
     ((lines "(a)" "" ";; about b" "(b)" "") '(:line 3) (lines "(a)" "" "(b)" ""))
     ;; at the end of the file: the blank lines before it go, the final newline stays
     ((lines "(a)" "" ";;; trailer" "") '(:comment 3) (lines "(a)" ""))
     ;; part of a block
     ((lines ";;; keep" ";;; drop" "(a)" "") '(:comment 2) (lines ";;; keep" "(a)" ""))
     ;; a #| |# block comment
     ((lines "(a)" "" "#| old" "   notes |#" "" "(b)" "") '(:comment 3) (lines "(a)" "" "(b)" ""))
     ;; inside a form
     ((lines "(defun f ()" "  ;; stale" "  (g))" "") '(:comment 2) (lines "(defun f ()" "  (g))" "")))
  (ok (equal (comment-op source locator #'delete-node) expected)))

(deftest-parametrize comment-block-replace
    ((source locator code expected)
     ((lines "(a)" "" ";;; Five sections" "" "(b)" "") '(:comment 3) ";;; Four sections"
      (lines "(a)" "" ";;; Four sections" "" "(b)" ""))
     ;; one comment line becomes two
     ((lines ";;; old" "(a)" "") '(:line 1) (lines ";;; new one" ";;; new two")
      (lines ";;; new one" ";;; new two" "(a)" ""))
     ;; a comment becomes code: one more top-level form
     ((lines "(a)" "" ";;; todo: b" "" "(c)" "") '(:comment 3) (lines ";;; b" "(defun b ()" "  2)")
      (lines "(a)" "" ";;; b" "(defun b ()" "  2)" "" "(c)" ""))
     ;; inside a form, at the comment's column
     ((lines "(defun f ()" "  ;; stale" "  (g))" "") '(:comment 2) (lines ";; fresh" ";; second")
      (lines "(defun f ()" "  ;; fresh" "  ;; second" "  (g))" "")))
  (ok (equal (comment-op source locator #'replace-node code) expected)))

(deftest comment-block-edits-are-checked
  (let* ((ps (read-source (lines "(a)" "" ";;; note" "" "(b)" "")))
         (block (locate ps '(:comment 3))))
    (ok (signals (replace-node ps block "(x") 'edit-input-unreadable) "balance check")
    (ok (signals (replace-node ps block "#| open") 'edit-input-unreadable))
    (ok (signals (replace-node ps block "  ") 'edit-input-unreadable) "nothing to write")
    (ok (signals (commit-plan (delete-node ps block) :dry-run t :expected-hash "stale")
                 'stale-source)
        "hash guard")
    (ok (commit-plan (delete-node ps block) :dry-run t :expected-hash (parsed-source-hash ps)))
    (ok (signals (wrap-node ps block "progn") 'edit-not-applicable))
    (ok (signals (splice-node ps block) 'edit-not-applicable))))

(deftest top-level-comment-blocks-are-listed
  (let ((blocks (comment-blocks (read-source *comment-source*))))
    (ok (equal (mapcar #'node-text blocks)
               (list (lines ";;; Header line one" ";;; Header line two") ";;; --- 3. mail")))))
