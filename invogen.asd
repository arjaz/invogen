(defsystem "invogen"
  :version "0.1.0"
  :author "Eugene Rossokha"
  :license ""
  :serial t
  :depends-on ("alexandria"
               "uiop"
               "mito"
               "cl-template"
               "unix-options")
  :components ((:module "src"
                :components
                ((:file "invogen"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "invogen"
  :entry-point "invogen:main"
  :in-order-to ((test-op (test-op "invogen/tests"))))

(defsystem "invogen/tests"
  :author "Eugene Rossokha"
  :license ""
  :depends-on ("invogen"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for invogen"
  :perform (test-op (op c) (symbol-call :rove :run c)))
