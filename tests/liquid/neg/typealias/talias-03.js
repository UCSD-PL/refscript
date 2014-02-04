/*@ predicate gt x y   = x >= y                */
/*@ alias gArray[x]    = [{number | gt(v, x)}] */

var ga /*@ gArray[1] */ = [0,0,0,0];
