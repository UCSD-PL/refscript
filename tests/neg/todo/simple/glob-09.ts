
// We shouldn't allow globals to apper in refinements

/*@ a :: { v: number | v > 0 } */
var a = 1;


var b = a + 1;

a = a + 1; 

var c = a + 1;

assert(b === c);
