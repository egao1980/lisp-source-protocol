(defsystem "lisp-source-protocol"
  :version "0.1.0"
  :description "Lossless Eclector CST, heuristic recovery and static analysis for Common Lisp source"
  :author "egao1980"
  :license "MIT"
  :depends-on ("eclector" "alexandria")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "text")
               (:file "tokens")
               (:file "nodes")
               (:file "client")
               (:file "scan")
               (:file "reader")
               (:file "locate")
               (:file "outline")
               (:file "packages")
               (:file "lint")
               (:file "edit")
               (:file "commit"))
  :in-order-to ((test-op (test-op "lisp-source-protocol/tests"))))

(defsystem "lisp-source-protocol/tests"
  :depends-on ("lisp-source-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "reader-test")
               (:file "recovery-test")
               (:file "locate-test")
               (:file "analysis-test")
               (:file "edit-test")
               (:file "corpus-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
