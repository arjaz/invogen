(asdf:load-system :invogen)
(in-package :invogen)

(sb-ext:save-lisp-and-die "invogen" :executable t :toplevel #'main)
