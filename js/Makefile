SCRIPTS=jquery.js jquery.csv.js d3.v2.js escodegen.browser.js angular.min.js radian.js html5slider.js app.js

all.js: all-uncomp.js
	uglifyjs all-uncomp.js > all.js

all-uncomp.js: $(SCRIPTS)
	cat $(SCRIPTS) > all-uncomp.js
