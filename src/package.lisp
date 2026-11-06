(defpackage #:lisp-source-protocol
  (:use #:cl)
  (:nicknames #:stack-lisp-source)
  (:export
   ;; conditions
   #:lisp-source-error
   #:locator-error
   #:locator-not-found
   #:ambiguous-locator
   #:locator-error-locator
   #:ambiguous-locator-candidates
   #:read-time-eval-refused
   #:read-time-eval-refused-node
   ;; text
   #:source-text
   #:make-source-text
   #:source-string
   #:offset-line-column
   #:line-start-offset
   #:line-count
   #:text-hash
   ;; tokens
   #:symbol-token
   #:symbol-token-p
   #:symbol-token-package
   #:symbol-token-name
   #:symbol-token-internal-p
   #:read-eval-placeholder
   #:read-eval-placeholder-p
   #:structure-placeholder
   #:structure-placeholder-p
   ;; nodes
   #:node
   #:expression-node
   #:trivia-node
   #:node-kind
   #:node-start
   #:node-end
   #:node-children
   #:node-parent
   #:node-value
   #:node-source
   #:node-recovered-p
   #:node-text
   #:node-forms
   #:node-path
   #:node-hash
   #:node-head-name
   #:node-symbol-name
   #:expression-node-p
   #:trivia-node-p
   #:walk-nodes
   ;; diagnostics / imbalance
   #:diagnostic
   #:diagnostic-severity
   #:diagnostic-kind
   #:diagnostic-message
   #:diagnostic-start
   #:diagnostic-end
   #:imbalance
   #:imbalance-kind
   #:imbalance-form-start
   #:imbalance-open-position
   #:imbalance-fix-position
   #:imbalance-count
   #:imbalance-confidence
   #:imbalance-message
   ;; parsed source
   #:parsed-source
   #:parsed-source-text
   #:parsed-source-nodes
   #:parsed-source-forms
   #:parsed-source-diagnostics
   #:parsed-source-imbalances
   #:parsed-source-path
   #:parsed-source-hash
   #:parsed-source-clean-p
   #:read-source
   #:read-source-file
   #:*source-features*
   ;; locate
   #:locate
   #:node-at-path
   #:node-at-offset
   #:find-definition-nodes
   ;; outline
   #:outline-entry
   #:outline-entry-kind
   #:outline-entry-name
   #:outline-entry-node
   #:outline-entry-docstring
   #:outline-entry-lambda-list
   #:outline-entry-package
   #:outline))
