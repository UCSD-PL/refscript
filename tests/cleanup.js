var glob = require("glob");
var readline = require('readline');
var exec = require('child_process').exec;
var exts = ["css", "vc", "hi", "out", "js", "json", "fqout", "fq", "o", "err", "annot", "log", "cgi", "smt2", "html", "liquid"];
var exceptions = ["cleanup.js", "tsconfig.json"];
var cmd = "find . " + exts.map(function (e) { return '-name "*.' + e + '" '; }).join(" -o ");
glob("**/.liquid", function (er, paths) {
    if (paths.length === 0) {
        console.log("No files found. Exiting...");
        return;
    }
    var questionText = "Are you sure you want to delete these " + paths.length + " paths? (type 'Yes' to confirm) ";
    console.log();
    paths.forEach(function (path) { return console.log(path); });
    console.log();
    var readLine = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    readLine.question(questionText, function (answer) {
        if (answer === "Yes") {
            console.log("Deleting " + paths.length + " paths ...");
            paths.forEach(function (path) { return exec('rm -r ' + path, function (err, stdout, stderr) { }); });
        }
        else {
            console.log("Aborting");
        }
        readLine.close();
    });
});
