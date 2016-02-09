
var junk = 0;

/* readonly arr :: # */

/*@  arr :: {a : Array<Immutable, { number | v < 3}> | len a > 0 } */
var arr =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo(): number {
  arr[0] = 2;
  var v = arr[0];
  return v + 1;
}

