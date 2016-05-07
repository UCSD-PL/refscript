
import glob     = require("glob");
import fs       = require('fs');
import readline = require('readline');
import path     = require('path');

let exec = require('child_process').exec;

// let extensions = ["css", "vc", "hi", "out", "js", "json", "fqout", "fq", "o", "err", "annot", "log", "cgi", "smt2", "html", "liquid"];
let exceptions = [];
let testDir = path.join(__dirname, "../tests");

glob(path.join(testDir, "**/.liquid"), function(er, paths) {
    // console.log(files);       
    if (paths.length === 0) {
        console.log("No files found. Exiting...");
        return;
    }

    var questionText = "Are you sure you want to delete these " + paths.length + " paths? (type 'Yes' to confirm) ";
    console.log();
    paths.forEach(path => console.log(path));    
    console.log();

    var readLine = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    readLine.question(questionText, function(answer: string) {
        if (answer === "Yes") {
            console.log("Deleting " + paths.length + " paths ...");
            paths.forEach(path => exec('rm -r ' + path, (err: any, stdout: string, stderr: string) => {}));
        }
        else {
            console.log("Aborting");
        }
        readLine.close();
    });
});

