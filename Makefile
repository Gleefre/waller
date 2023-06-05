LISP ?= sbcl
APP = waller

all: clean build

build:
	cat make-waller.lisp | $(LISP)
	cat make-waller-editor.lisp | $(LISP)

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
