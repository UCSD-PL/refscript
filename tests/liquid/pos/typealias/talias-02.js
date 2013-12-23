/*@ predicate gt x y  = x >= y              */
/*@ alias nat         = {number | gt(v, 0)} */

/*@ type Poo[A](x,y)  = {A | v = x + y }    */
/*@ type Foo A x y    = {A | v = x + y }    */

/*@ z :: nat */
var z = 12;
