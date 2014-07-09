/// <reference path="../underscore/underscore.d.ts" />

var root:RootStatic;
var exports:any; 
var module:any;


function foo(){
  if (typeof exports !== 'undefined') {
    if (typeof module !== 'undefined' && module.exports) {
      exports = module.exports = _;
    }
    exports._ = _;
  } else {
    root._ = _;
  }

  //root.bogzert(10);
}

function bar(x:number):number[] { 
  var r = [x];
  r.push(1);
  return r;
}

