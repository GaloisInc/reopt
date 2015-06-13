all:

tmp:
	mkdir -p tmp

etags: tmp
	hasktags --ignore-close-implementation --etags --tags-absolute --output=tmp/TAGS src deps/*/src

clean:
	-rm -rf tmp
