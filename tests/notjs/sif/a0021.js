var x = lSLoV(UNKNOWN_NUMBER, "private");
var foo; 
var z = 0;

function maker(p) {
 if (p == (4-1)) {
   return function () { z = 1; };
 } else {
   return function() { z = 2; };
 }
}

maker(x)();
z
