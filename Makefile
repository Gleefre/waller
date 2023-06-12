LISP ?= sbcl
APP = waller

all: clean build

game:
	cat make-waller.lisp | $(LISP)

editor:
	cat make-waller-editor.lisp | $(LISP)

build: game editor

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
	cp run.sh $(APP)
	cp run-editor.sh $(APP)
	cp run.bat $(APP)
	cp run-editor.bat $(APP)

lin-bundle: bundle
	zip -r $(APP)-lin $(APP)

win-bundle: bundle
