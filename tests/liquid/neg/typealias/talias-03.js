/*@ predicate gt x y   = x >= y                */
/*@ type gArray[x]     = [{number | gt(v, x)}] */

var ga /*@ gArray[1] */ = [0,0,0,0];
