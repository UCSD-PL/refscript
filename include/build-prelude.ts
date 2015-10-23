
declare function require(x: string): any;
declare var __dirname: string;

var exec = require('child_process').exec;
var path = require('path');

var outputFile = path.join(__dirname, "./prelude.d.ts");

var files = [
    "./rsc/prims.d.ts",
    "./rsc/mutability.d.ts",
    "./rsc/aliases.d.ts",
    "./rsc/qualifiers.d.ts",
    "./rsc/measures.d.ts",
    "./ambient/undefined.d.ts",
    "./ambient/object.d.ts",
    "./ambient/array.d.ts",
    "./ambient/boolean.d.ts",
    "./ambient/function.d.ts",
    "./ambient/string.d.ts",
    "./ambient/number.d.ts",
    "./ambient/iarguments.d.ts",
    "./ambient/regexp.d.ts",
    "./ambient/error.d.ts"
].map(f => path.join(__dirname, f));

var fileString = files.join(" ");

exec('cat ' + fileString + ' > ' + outputFile);
