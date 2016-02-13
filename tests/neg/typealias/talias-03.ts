/*@ predicate gt x y   = x >= y                */
/*@ type gArray[x]    = IArray<{number | gt(v, x)}> */

/*@ ga :: #gArray[1] */
let ga = [0,0,0,0];
