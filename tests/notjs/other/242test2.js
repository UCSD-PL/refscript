
//Example from CS242 at Stanford.
//toString is implicitly called; should return "a10"
//As of 9/23/2012 this is buggy in the s.s. abstract interpreter.

var y = "a"
var x = { toString : function() { return y; }}
x = x + 10;
x;
