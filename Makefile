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

lin-bundle: bundle
	cp run.sh $(APP)
	cp run-editor.sh $(APP)
	if [ -f $(APP)/bin/libzstd.so.1.* ]; then mv $(APP)/bin/libzstd.so.1.* $(APP)/bin/libzstd.so.1; fi
	if [ -f $(APP)/bin/libz.so.1.* ]; then mv $(APP)/bin/libz.so.1.* $(APP)/bin/libz.so.1; fi
	zip -r $(APP)-lin $(APP)

win-bundle: bundle
	cp run.bat $(APP)
	cp run-editor.bat $(APP)
