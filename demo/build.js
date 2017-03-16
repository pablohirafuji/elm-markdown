var fs = require('fs');
var path = require("path");
var exec = require('child_process').exec;
var makeCmd = 'elm-make Main.elm --output ../main.js';
var uglifyCmd = 'uglifyjs ../main.js --output ../main.js';
var index = fs.readFileSync(path.resolve(__dirname + "/index.html"));
var indexStr = index.toString();

exec(makeCmd, elm_make);


function elm_make (error, stdout, stderr) {
	if (error) {
		console.error(error);
		return;
	}

	console.log(stdout);
	exec(uglifyCmd, uglify);
};

function uglify (error, stdout, stderr) {
	if (error) {
		console.error(error);
		return;
	}

	console.log("Successfully uglifyed main.js");
	writeIndexHtml()
};


function writeIndexHtml () {
	var newIndex = indexStr.replace("/_compile/Main.elm", "main.js");
	fs.writeFileSync("../index.html", newIndex);
}
