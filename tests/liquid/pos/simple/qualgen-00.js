
/*@ z :: {number | (0 < v && v < 3)} */
var junk = 2;

var arr  =  [1, 2];

/*@ foo :: () => { number | v < 4 } */
function foo() {
  arr[0] = 2;
  return arr[0] + 1;
}

