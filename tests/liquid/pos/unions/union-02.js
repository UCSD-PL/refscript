


/*@ a :: { number | true } */
var a  = 1;

/*@ b :: string */
var b  = "a";

/*@ c :: { number + boolean | true} */
var c  = 1;

/*@ d :: { { number | v = 1 } + string | true } */
var d  = 1;

/*@ e :: { {  } | true } */
var e  = { } ;

/*@ f :: { { d: { number + boolean | true }  } | true } */
var f  = { d: 1 };
