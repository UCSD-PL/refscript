/*@ predicate gt x y  = x < y          */
/*@ type gnat[A,x]    = {A | (gt v x)}  */
/*@ type nat          = gnat[number, 0] */

// thus `nat` is just an alias for: {number | (gt v 0)} 

var z /*@ nat */ = 12;
