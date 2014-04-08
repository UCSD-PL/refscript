
/*@ qualif Num(v : number): (v < 3) */

/*@ junk :: { number | (0 < v && v < 40) } */
var junk = 2;

/*@ arr :: [ number ] */
var arr =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo(): number {
  arr[0] = 2;
  return arr[0] + 1;
}

