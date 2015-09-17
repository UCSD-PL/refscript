
/*@ a :: posint */
var a = 1;
var b = a + 1;
a = a + 1;
var c = a + 1;
assert(b === c);
