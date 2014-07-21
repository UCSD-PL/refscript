#!/usr/bin/env node
var fs = require('fs');
var sys = require('sys');
var readline = require('readline');

var exts=["css", "vc", "hi", "out", "js", "json", "fqout",
          "fq", "o", "err", "annot", "log", "cgi", "smt2", "html"];

var exceptions=["cleanup.js"]

var cmd = "find . " + exts.map(function(e){return '-name "*.' + e + '" '}).join(" -o ");

var exec = require('child_process').exec;
function puts(error, stdout, stderr) { 
  files = stdout.split("\n").filter(function(s){return s !== ""; });

  //Filter out exceptions
  exceptions.forEach(function(e) {
    files = files.filter(function(f){ 
      return f.indexOf(e) === -1;
    });
  });

  files.forEach(function(f){console.log(f);});

  if (files.length === 0) {
    console.log("No files found. Exiting...");
    return;
  }

  var txt = "\nAre you sure you want to delete these " + files.length + " files? (type 'Yes' to confirm) ";
  
  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  rl.question(txt, function(answer) {
    if (answer === "Yes") {
      console.log("Delerting " + files.length + " files ...");
      files.forEach(function(f){ fs.unlink(f);});
    }
    else {
      console.log("Aborting");
    }
    rl.close();
  });
}

exec(cmd, puts);

