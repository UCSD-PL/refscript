
// Globals are not allowed to apper in refinements

/*@ [global] a :: posint */
var a = 1;

/*@ [global] c :: posint */
var c = 1;

a = a - 5;

/*@ b :: { v: number | v > a && v >= c }  */
var b = 2;
