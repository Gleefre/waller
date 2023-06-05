LISP ?= sbcl
APP = waller
APP_2 = waller/editor

all: clean build

build:
	$(LISP) --eval "(ql:quickload '(:deploy :sketch :harmony))" \
		--eval "(push :deploy *features*)" \
		--load $(APP).asd \
		--eval "(ql:quickload :$(APP))" \
		--eval "(asdf:load-system :$(APP) :force T)" \
		--eval "(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)" \
		--eval "(deploy:define-resource-directory data \"res/\")" \
		--eval "(asdf:make :$(APP))" \
		--quit
	$(LISP) --eval "(ql:quickload '(:deploy :sketch :harmony))" \
		--eval "(push :deploy *features*)" \
		--eval "(push :deploy-console *features*)" \
		--load $(APP).asd \
		--eval "(ql:quickload :$(APP))" \
		--eval "(asdf:load-system :$(APP_2) :force T)" \
		--eval "(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)" \
		--eval "(deploy:define-resource-directory data \"res/\")" \
		--eval "(asdf:make :$(APP_2))" \
		--quit

clean:
	rm -rf $(APP)
	rm -rf bin
	rm -rf $(APP)-win.zip
	rm -rf $(APP)-lin.zip

bundle: all
	mkdir $(APP)
	mv bin $(APP)/
	cp LICENSE $(APP)
	cp NOTICE $(APP)

lin-bundle: bundle
	cp run.sh $(APP)
	cp run-editor.sh $(APP)
	zip -r $(APP)-lin $(APP)

win-bundle: bundle
	cp run.bat $(APP)
	cp run-editor.bat $(APP)
