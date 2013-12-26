
/*@ junk :: {number | (0 < v && v < 30)} */
var junk = 2;


var arr  =  [1, 2];


/*@ foo :: () => { number | v < 40 } */
function foo() {
  arr[0] = 2;
  return arr[0] + 1;
}

