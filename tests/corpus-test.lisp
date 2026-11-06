(in-package #:lisp-source-protocol/tests)

;;; Corpus: this system's own sources, plus every .lisp/.asd under
;;; $LISP_SOURCE_CORPUS when set (e.g. the whole workspace). Hidden
;;; directories (.qlot, .cl-repository, …) are skipped.

(defun own-files ()
  (let ((root (asdf:system-source-directory "lisp-source-protocol")))
    (append (directory (merge-pathnames "src/*.lisp" root))
            (directory (merge-pathnames "tests/*.lisp" root))
            (directory (merge-pathnames "*.asd" root)))))

(defun hidden-path-p (path)
  (some (lambda (part) (and (stringp part) (plusp (length part)) (char= (char part 0) #\.)))
        (rest (pathname-directory path))))

(defun corpus-files ()
  (let ((extra (uiop:getenv "LISP_SOURCE_CORPUS")))
    (append (own-files)
            (when (and extra (plusp (length extra)))
              (let ((dir (uiop:ensure-directory-pathname extra)))
                (remove-if #'hidden-path-p
                           (append (directory (merge-pathnames "**/*.lisp" dir) :resolve-symlinks nil)
                                   (directory (merge-pathnames "**/*.asd" dir) :resolve-symlinks nil))))))))

(deftest corpus-is-covered
  (let ((files (corpus-files))
        (uncovered '()))
    (dolist (f files)
      (when (first-uncovered (read-source-file f))
        (push (namestring f) uncovered)))
    (ok (plusp (length files)))
    (ok (null uncovered) (format nil "~D files fully covered~@[; not: ~{~A~^, ~}~]"
                                 (length files) (subseq uncovered 0 (min 5 (length uncovered)))))))

(deftest own-sources-read-clean
  (let ((unclean (loop for f in (own-files)
                       unless (parsed-source-clean-p (read-source-file f))
                         collect (namestring f))))
    (ok (null unclean) (format nil "own sources clean~@[; not: ~{~A~^, ~}~]" unclean))))

;;; Mutation: damage one paren inside a top-level form and check the other
;;; top-level forms survive byte-identical.

(defun mutate (string offset kind)
  (ecase kind
    (:delete (concatenate 'string (subseq string 0 offset) (subseq string (1+ offset))))
    (:insert (concatenate 'string (subseq string 0 offset) ")" (subseq string offset)))))

(defun mutation-sites (ps)
  "Offsets of inner parens of multi-line top-level lists: (offset . kind)."
  (let ((sites '())
        (s (source-string (parsed-source-text ps))))
    (dolist (f (parsed-source-forms ps))
      (when (and (eq (node-kind f) :list)
                 (find #\Newline s :start (node-start f) :end (node-end f)))
        (walk-nodes (lambda (n)
                      (when (and (eq (node-kind n) :list) (not (eq n f)))
                        (push (cons (1- (node-end n)) :delete) sites)
                        (push (cons (node-start n) :insert) sites)))
                    (list f))))
    (nreverse sites)))

(deftest mutations-stay-local
  (let ((seed 1980)
        (trials 0) (local 0) (detected 0) (near 0))
    (dolist (f (corpus-files))
      (let* ((string (uiop:read-file-string f))
             (ps (read-source string)))
        (when (parsed-source-clean-p ps)
          (let ((sites (coerce (mutation-sites ps) 'vector)))
            (dotimes (k (min 10 (length sites)))
              (destructuring-bind (offset . kind) (aref sites (mod (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (length sites)))
                (let* ((owner (find-if (lambda (g) (< (node-start g) offset (node-end g)))
                                       (parsed-source-forms ps)))
                       (mp (read-source (mutate string offset kind)))
                       (mtexts (texts mp))
                       (imbalance (first (parsed-source-imbalances mp))))
                  (incf trials)
                  (when (every (lambda (g) (or (eq g owner)
                                               (member (node-text g) mtexts :test #'string=)))
                               (parsed-source-forms ps))
                    (incf local))
                  (when imbalance
                    (incf detected)
                    (when (<= (abs (- (offset-line-column (parsed-source-text mp)
                                                          (imbalance-fix-position imbalance))
                                      (offset-line-column (parsed-source-text ps) offset)))
                              2)
                      (incf near))))))))))
    (ok (plusp trials))
    (ok (>= (/ local trials) 99/100) (format nil "locality ~D/~D" local trials))
    (ok (>= (/ detected trials) 99/100) (format nil "detected ~D/~D" detected trials))
    (ok (>= (/ near trials) 3/4) (format nil "fix within 2 lines ~D/~D" near trials))))
