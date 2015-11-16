#!/usr/bin/env node
var fs = require('fs');
var sys = require('sys');
var readline = require('readline');

var exts=["css", "vc", "hi", "out", "js", "json", "fqout",
          "fq", "o", "err", "annot", "log", "cgi", "smt2", "html", "liquid"];

var exceptions=["cleanup.js", "tsconfig.json"]

var cmd = "find . " + exts.map(function(e){return '-name "*.' + e + '" '}).join(" -o ");

// http://www.geedew.com/2012/10/24/remove-a-directory-that-is-not-empty-in-nodejs/
var deleteFolderRecursive = function(path) {
  if( fs.existsSync(path) ) {
    fs.readdirSync(path).forEach(function(file,index){
      var curPath = path + "/" + file;
      if(fs.lstatSync(curPath).isDirectory()) { // recurse
        deleteFolderRecursive(curPath);
      } else { // delete file
        fs.unlinkSync(curPath);
      }
    });
    fs.rmdirSync(path);
  }
};

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

  var txt = "\nAre you sure you want to delete these " + files.length + " paths? (type 'Yes' to confirm) ";

  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  rl.question(txt, function(answer) {
    if (answer === "Yes") {
      console.log("Deleting " + files.length + " paths ...");

      files.forEach(function(f){
        if (fs.lstatSync(f).isFile()) {
          fs.unlink(f);
        }
      });

      files.forEach(function(f){
        if (fs.existsSync(f) && fs.lstatSync(f).isDirectory()) {
          deleteFolderRecursive(f);
        }
      });


    }
    else {
      console.log("Aborting");
    }
    rl.close();
  });
}

exec(cmd, puts);
