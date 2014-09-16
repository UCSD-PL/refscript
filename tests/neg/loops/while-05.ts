/*@ foo :: (n: number) => { boolean | true } */
function foo(n: number) {
  var b; 
  var i;
  var j = 0;
  while (j < n) {
    b = i < 10;
    i = j;
    j++;
  }
  return b;
}

