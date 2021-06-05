LISP?=sbcl

build:
	$(LISP) --load build.lisp

sbcl:
	sbcl \
		--load src/invogen.lisp \
		--eval "(in-package :invogen)" \
		--eval "(sb-ext:save-lisp-and-die #p\"invogen\" :toplevel #'invogen:main :executable t)"

clean:
	rm invogen
	find src -type f -name *.fasl -exec ls '{}' \;
