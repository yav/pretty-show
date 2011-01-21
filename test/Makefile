example.html: test.html body.html
	cpp -P $< > $@

body.html: example
	ppsh --html < $< > $@
