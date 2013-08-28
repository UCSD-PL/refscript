var x;

function foo() { x = 1; }
function bar() { x = "str"; }
function blah() { x = true; }

function oracle() { return 1 < 2; }

var func = blah;

if (oracle()) 
  func = foo;
else 
  func = bar; 

func();
x
