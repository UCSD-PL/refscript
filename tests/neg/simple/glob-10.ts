
// We shouldn't allow globals to apper in refinements

/*@ a :: { v: number | v > 0 } */
var a = 1;

/*@ c :: { v: number | v > 0 } */
var c = 1;

a = a - 5; 

/*@ b :: { v: number | (v > a) && ( v >= c) }  */
var b = 2;

