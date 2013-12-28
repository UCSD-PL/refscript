
var arr /*@ [ { number |( (v > 0)  && (v < 3))} ] */ =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo() {
  return arr[0] + 1;
}

