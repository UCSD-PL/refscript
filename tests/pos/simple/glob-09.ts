
// We shouldn't allow globals to apper in refinements

/*@ a :: { v: number | v > 0 } */
var a = 1;

var b = a + 1;

var c = b;

assert(b === c);
