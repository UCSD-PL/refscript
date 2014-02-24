/*@ qualif LT3(v:number): v < 3 */

var arr :number[] /*@ { [ { number |( (v > 0)  && (v < 3))} ] | (len v) = 2 } */ =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo() :number{
  return arr[0] + 1;
}

