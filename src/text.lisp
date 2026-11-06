(in-package #:lisp-source-protocol)

(defclass source-text ()
  ((string :initarg :string :reader source-string :type simple-string)
   (line-starts :initarg :line-starts :reader source-line-starts :type simple-vector))
  (:documentation "Immutable source string plus a table of line start offsets."))

(defun make-source-text (string)
  (let ((string (coerce string 'simple-string))
        (starts (list 0)))
    (loop for i from 0 below (length string)
          when (char= (schar string i) #\Newline)
            do (push (1+ i) starts))
    (make-instance 'source-text
                   :string string
                   :line-starts (coerce (nreverse starts) 'simple-vector))))

(defun line-count (text)
  (length (source-line-starts text)))

(defun offset-line (text offset)
  "0-based line index containing OFFSET."
  (let* ((starts (source-line-starts text))
         (lo 0)
         (hi (1- (length starts))))
    (declare (type fixnum lo hi))
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (svref starts mid) offset)
                   (setf lo mid)
                   (setf hi (1- mid)))))
    lo))

(defun offset-line-column (text offset)
  "Return 1-based line and 0-based column of OFFSET."
  (let ((line (offset-line text offset)))
    (values (1+ line) (- offset (svref (source-line-starts text) line)))))

(defun line-start-offset (text line)
  "Offset of the first character of 1-based LINE."
  (svref (source-line-starts text) (1- line)))

(defun line-end-offset (text line)
  "Offset of the newline ending 1-based LINE (or the string length)."
  (let ((starts (source-line-starts text)))
    (if (< line (length starts))
        (1- (svref starts line))
        (length (source-string text)))))

(defun text-hash (string)
  "Stable content hash (FNV-1a 64, hex) of STRING."
  (let ((h #xcbf29ce484222325))
    (declare (type (unsigned-byte 64) h))
    (loop for ch across string
          do (setf h (logand #xffffffffffffffff
                             (* (logxor h (char-code ch)) #x100000001b3))))
    (format nil "~(~16,'0x~)" h)))
