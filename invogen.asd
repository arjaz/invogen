(asdf:defsystem #:invogen
  :version "0.1.0"
  :author "Eugene Rossokha"
  :license ""
  :serial t
  :depends-on (#:alexandria
               #:uiop
               #:mito
               #:cl-template
               #:unix-opts
               #:iterate)
  :components ((:file "invogen"))
  :description ""
  :build-operation "program-op"
  :build-pathname "invogen"
  :entry-point "invogen:main")
