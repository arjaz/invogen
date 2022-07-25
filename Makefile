LISP ?= sbcl

build:
	$(LISP) --non-interactive \
	--load invogen.asd \
	--eval '(ql:quickload :invogen)' \
	--eval '(asdf:make :invogen)'
