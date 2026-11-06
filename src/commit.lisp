(in-package #:lisp-source-protocol)

;;; Diffs and atomic writes for verified plans.

(defun split-lines (string)
  (let ((lines '()) (start 0))
    (loop for nl = (position #\Newline string :start start)
          do (push (subseq string start (or nl (length string))) lines)
             (if nl (setf start (1+ nl)) (return)))
    (coerce (nreverse lines) 'simple-vector)))

(defun lcs-script (a b)
  "Edit script between line vectors A and B: list of (:same i j) (:del i) (:add j)."
  (let* ((n (length a)) (m (length b))
         (table (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (loop for i from (1- n) downto 0
          do (loop for j from (1- m) downto 0
                   do (setf (aref table i j)
                            (if (string= (svref a i) (svref b j))
                                (1+ (aref table (1+ i) (1+ j)))
                                (max (aref table (1+ i) j) (aref table i (1+ j)))))))
    (let ((i 0) (j 0) (out '()))
      (loop while (or (< i n) (< j m))
            do (cond ((and (< i n) (< j m) (string= (svref a i) (svref b j)))
                      (push (list :same i j) out) (incf i) (incf j))
                     ((and (< j m) (or (= i n) (>= (aref table i (1+ j)) (aref table (1+ i) j))))
                      (push (list :add j) out) (incf j))
                     (t (push (list :del i) out) (incf i))))
      (nreverse out))))

(defun unified-diff (old new &key (path "source") (context 2))
  "Unified diff of OLD → NEW strings (line based). Empty string when equal."
  (when (string= old new) (return-from unified-diff ""))
  (let* ((a (split-lines old)) (b (split-lines new))
         (prefix (loop for k from 0 below (min (length a) (length b))
                       while (string= (svref a k) (svref b k)) count t))
         (suffix (loop for k from 1 to (- (min (length a) (length b)) prefix)
                       while (string= (svref a (- (length a) k)) (svref b (- (length b) k)))
                       count t))
         (mid-a (subseq a prefix (- (length a) suffix)))
         (mid-b (subseq b prefix (- (length b) suffix)))
         (script (if (<= (* (length mid-a) (length mid-b)) 4000000)
                     (mapcar (lambda (op)
                               (ecase (first op)
                                 (:same (list :same (+ prefix (second op)) (+ prefix (third op))))
                                 (:del (list :del (+ prefix (second op))))
                                 (:add (list :add (+ prefix (second op))))))
                             (lcs-script mid-a mid-b))
                     (append (loop for i from prefix below (- (length a) suffix) collect (list :del i))
                             (loop for j from prefix below (- (length b) suffix) collect (list :add j)))))
         (full (append (loop for k below prefix collect (list :same k k))
                       script
                       (loop for k from suffix downto 1
                             collect (list :same (- (length a) k) (- (length b) k)))))
         (ops (coerce full 'simple-vector))
         (changed (loop for k below (length ops) unless (eq (first (svref ops k)) :same) collect k)))
    (with-output-to-string (out)
      (format out "--- a/~A~%+++ b/~A~%" path path)
      ;; group changed op indices into hunks with CONTEXT ops around them
      (loop while changed
            do (let* ((lo (max 0 (- (first changed) context)))
                      (hi (first changed)))
                 (loop while (and changed (<= (first changed) (+ hi (* 2 context))))
                       do (setf hi (pop changed)))
                 (setf hi (min (1- (length ops)) (+ hi context)))
                 (let ((a-start nil) (b-start nil) (a-count 0) (b-count 0) (body '()))
                   (loop for k from lo to hi
                         for op = (svref ops k)
                         do (ecase (first op)
                              (:same (unless a-start (setf a-start (second op)))
                               (unless b-start (setf b-start (third op)))
                               (incf a-count) (incf b-count)
                               (push (format nil " ~A" (svref a (second op))) body))
                              (:del (unless a-start (setf a-start (second op)))
                               (incf a-count)
                               (push (format nil "-~A" (svref a (second op))) body))
                              (:add (unless b-start (setf b-start (second op)))
                               (incf b-count)
                               (push (format nil "+~A" (svref b (second op))) body))))
                   (format out "@@ -~D,~D +~D,~D @@~%~{~A~%~}"
                           (1+ (or a-start 0)) a-count (1+ (or b-start 0)) b-count
                           (nreverse body))))))))

(defun write-file-atomically (path string)
  "Write STRING (UTF-8) next to PATH, then rename over it."
  (let ((tmp (make-pathname :name (format nil ".~A.~D.tmp" (pathname-name path)
                                          (random 1000000 (make-random-state t)))
                            :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (write-string string out))
    (uiop:rename-file-overwriting-target tmp path)
    path))

(defclass edit-result ()
  ((plan :initarg :plan :reader edit-result-plan)
   (parsed :initarg :parsed :reader edit-result-parsed
           :documentation "The re-read source after the edit.")
   (diff :initarg :diff :reader edit-result-diff)
   (written :initarg :written :reader edit-result-written-p)))

(defmethod print-object ((r edit-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~(~A~)~:[ (dry run)~;~] → ~A"
            (edit-plan-operation (edit-result-plan r)) (edit-result-written-p r)
            (parsed-source-hash (edit-result-parsed r)))))

(defun commit-plan (plan &key dry-run expected-hash)
  "Verify PLAN, then (unless DRY-RUN) write it to the source's path.
Signals STALE-SOURCE when EXPECTED-HASH or the file on disk no longer
matches the parsed text. Returns an EDIT-RESULT."
  (let* ((parsed (edit-plan-parsed plan))
         (path (parsed-source-path parsed)))
    (check-expected-hash parsed expected-hash)
    (let ((new (verify-plan plan)))
      (when (and path (not dry-run) (probe-file path))
        (let ((on-disk (text-hash (alexandria:read-file-into-string path :external-format :utf-8))))
          (check-expected-hash parsed on-disk)))
      (when (and path (not dry-run))
        (write-file-atomically path (source-string (parsed-source-text new))))
      (make-instance 'edit-result
                     :plan plan :parsed new :written (and path (not dry-run) t)
                     :diff (unified-diff (source-string (parsed-source-text parsed))
                                         (source-string (parsed-source-text new))
                                         :path (if path (file-namestring path) "source"))))))
