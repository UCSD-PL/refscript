
/*
    TODO:
    - use something like http://jolira.github.io/backgrounder/ to parallelize process
*/

// Imports
import child_process = require('child_process');
let    glob          = require("glob-promise");
let    microtime     = require('microtime');
let    colors        = require('colors');
import _             = require('underscore');
import Promise       = require('bluebird');
import path          = require('path');


// Definitions

type Map<A> = { [x: string]: A }
type Path = string;
type Query = string;

enum ExitCode {
    EXIT_SUCCESS,
    EXIT_FAILURE,
    EXIT_UNDEFINED
}

declare module String {
    export var format: any;
}

interface MyError {
    pid: number;
    output: string[];
    stdout: string | Buffer;
    stderr: string | Buffer;
    status: number;
    signal: string;
    error: Error;
}

class Pair<A, B> {
    constructor(public x: A, public y: B) {

    }
}

interface TestQuery {
    code: ExitCode;
    query: Query;
}

interface TestFile {
    code: ExitCode;
    path: Path;
}


// Main code

let exec = child_process.exec;
let commandPrefix = ['stack exec -- rsc']

let testDir = path.join(__dirname, "../tests");
let polarities = ["pos", "neg"];
let categories = [
    "operators",
    "simple",
    "scope",
    "arrays"
];

let testQueries: TestQuery[] = _.flatten(polarities.map(polarity => categories.map(category =>
    makeTestQuery(polarity, path.join(testDir, polarity, category, "**/*.ts"))
)));

let failures: Path[] = [];
let exceptions: Path[] = [];

let testPromises = testQueries.map(testFolder => {
    return toPromise(testFolder);
});

Promise.all(Promise.map(testPromises, (testFiles_, i, l) => testFiles_)).then(testFiles_ => {
    let testFiles: TestFile[] = _.flatten(testFiles_);
    testFiles.forEach(run);
    postProcess();
});



// Functions

function toPromise(fld: TestQuery): Promise.Thenable<TestFile[]> {
    return glob(fld.query).then((paths: Path[]) => paths.map(path => makeTestFile(fld.code, path)));
}

function makeTestQuery(polarity: string, query: Query): TestQuery {
    return {
        code: polarityToExitCode(polarity),
        query: query
    }
}

function makeTestFile(code: ExitCode, path: Path): TestFile {
    return {
        code: code,
        path: path
    }
}

function polarityToExitCode(polarity: string) {
    switch (polarity) {
        case "pos":
            return ExitCode.EXIT_SUCCESS;
        case "neg":
            return ExitCode.EXIT_FAILURE;
        default:
            return ExitCode.EXIT_UNDEFINED;
    }
}

function statusToExitCode(status: number) {
    switch (status) {
        case 0:
            return ExitCode.EXIT_SUCCESS;
        case 1:
            return ExitCode.EXIT_FAILURE;
        default:
            return ExitCode.EXIT_UNDEFINED;
    }
}

function run(testFile: TestFile) {
    let file = testFile.path;
    let command = commandPrefix.concat([file]).join(' ');
    let startTime = microtime.now();
    try {
        let _ = child_process.execSync(command);
        let elapsedTimeMs = (microtime.now() - startTime) / 1000;
        console.log('%s [%d ms] %s', file, elapsedTimeMs, 'OK'.bold.green);
    } catch (e) {
        let elapsedTimeMs = (microtime.now() - startTime) / 1000;
        let error: MyError = e;
         if (statusToExitCode(error.status) === testFile.code) {
            console.log('%s [%d ms] %s', file, elapsedTimeMs, 'OK'.bold.green);
        }
        else if (ExitCode[error.status]) {
            console.log('%s [%d ms] %s', file, elapsedTimeMs, 'FAIL'.bold.red);
            failures.push(file);
        }
        else {
            console.log('%s [%d ms] %s', file, elapsedTimeMs, 'EXCEPTION'.bold.grey);
            exceptions.push(file);
        }
    }
}

function postProcess() {
    if (failures.length > 0) {
        console.log();
        console.log((failures.length + " FAILURES").bold.red);
        failures.forEach(f => console.log(f));
    }

    if (exceptions.length > 0) {
        console.log();
        console.log((exceptions.length + " EXCEPTIONS").bold.red);
        exceptions.forEach(f => console.log(f));
    }
}
