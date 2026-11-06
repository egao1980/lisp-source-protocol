(in-package #:lisp-source-protocol)

(define-condition lisp-source-error (error) ()
  (:documentation "Root of lisp-source-protocol errors."))

(define-condition locator-error (lisp-source-error)
  ((locator :initarg :locator :reader locator-error-locator)))

(define-condition locator-not-found (locator-error) ()
  (:report (lambda (c s)
             (format s "No form matches locator ~S" (locator-error-locator c)))))

(define-condition ambiguous-locator (locator-error)
  ((candidates :initarg :candidates :reader ambiguous-locator-candidates))
  (:report (lambda (c s)
             (format s "Locator ~S matches ~D forms"
                     (locator-error-locator c)
                     (length (ambiguous-locator-candidates c))))))

(define-condition read-time-eval-refused (condition)
  ((node :initarg :node :reader read-time-eval-refused-node))
  (:documentation "Signalled (not an error) for each #. form. The form is never evaluated.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Read-time evaluation (#.) refused"))))
