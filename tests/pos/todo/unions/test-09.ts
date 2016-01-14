
/*@ a :: { number | 0 < 1 } */
let a : number  = 1;

/*@ b :: string */
let b : string = "a";

/*@ c :: number + boolean */
let c  = 1;

/*@ d :: { v: number | v = 1 } + { v: string | v = "" } */
let d  = 1;

/*@ e :: { {  } | 0 < 1 } */
let e  = { } ;

/*@ f :: { d: number + boolean } */
let f  = { d: 1 };
