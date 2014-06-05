/*@ qualif LT3(v:number): v < 3 */

/*@ arr :: { #Array[#Immutable, { number |( (v > 0)  && (v < 3))} ] | (len v) = 2 } */ 
var arr :number[] =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo() :number{
  return arr[0] + 1;
}

