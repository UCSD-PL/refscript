
var f = function() {};
var foo = function() { };

foo["prototype"].x = 1;

var y = new foo();

y instanceof f;

