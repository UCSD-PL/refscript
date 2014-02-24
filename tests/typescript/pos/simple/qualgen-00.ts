var junk :number /*@ {number | (0 < v && v < 30)} */ = 2;

var arr :number[] =  [1, 2];

/*@ foo :: () => { number | v < 40 } */
function foo():number {
  arr[0] = 2;
  return arr[0] + 1;
}

