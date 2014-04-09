/*@ predicate gt x y   = x >= y                */
/*@ alias gArray[x]    = [{number | gt(v, x)}] */

/*@ ga :: #gArray[1] */
var ga = [0,0,0,0];
