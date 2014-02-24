/*@ predicate gt x y  = x < y          */
/*@ alias gnat[A,x]   = {A | (gt v x)}  */
/*@ alias nat         = gnat[number, 0] */

// thus `nat` is just an alias for: {number | (gt v 0)} 
/*@ z :: nat */
var z = 12;
