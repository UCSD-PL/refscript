
/*@ a :: { number | true } */
var a : number  = 1;

/*@ b :: string */
var b : string = "a";

/*@ c :: { number + boolean | true} */
var c  = 1;

/*@ d :: { v: number | v = 1 } + { v: string | v = "" } */
var d  = 1;

/*@ e :: { {  } | true } */
var e  = { } ;

/*@ f :: { d: { number + boolean | true }  } */
var f  = { d: 1 };
