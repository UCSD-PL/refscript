
//From Stanford CS242 on 9/23/2012
//Passes.

var f = function() {
  var a = g();
  function g() { return 1; }
  function g() { return 2; }
  g = function() { return 3; }
  return a;
}

f() //should return 2
