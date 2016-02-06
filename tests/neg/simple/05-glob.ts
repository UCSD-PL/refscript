
// Globals are not allowed to apper in refinements

/*@ a :: posint */
let a = 1;

/*@ c :: posint */
let c = 1;

a = a - 5;

/*@ b :: { v: number | v > a && v >= c }  */
let b = 2;
