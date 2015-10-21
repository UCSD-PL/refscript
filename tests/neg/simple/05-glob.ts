
// Globals are not allowed to apper in refinements

/*@ [global] a :: posint */
let a = 1;

/*@ [global] c :: posint */
let c = 1;

a = a - 5;

/*@ b :: { v: number | v > a && v >= c }  */
let b = 2;
