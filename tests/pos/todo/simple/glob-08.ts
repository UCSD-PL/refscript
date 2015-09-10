
/*@ a :: { v: number | v > 0 } */
var a = 1;

/*@ b :: { v: number | v > 0 } */
var b = 1;

/*@ c :: { v: number | v > 0 }  */
var c = a + b;

