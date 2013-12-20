/*@ qualif LT3(v:number): v < 3 */

/*@ arr :: [ { number |( (v > 0)  && (v < 3))} ] */
var arr =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo() {
  return arr[0] + 1;
}

