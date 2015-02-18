
/*@ qualif Num(v : number): (v < 3) */
/* qualif Len(v : number): (0 < (len v)) */

/* junk :: { number | (0 < v && v < 40) } */
var junk = 0;

var arr /*@ readonly */ =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo(): number {
  arr[0] = 2;
  var v = arr[0];
  return v + 1;
}

