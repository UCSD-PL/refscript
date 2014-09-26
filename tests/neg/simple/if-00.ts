
/*@ foo :: (a: number) => { number | true } */
function foo(a: number): number {
  var i = 0;
  if (a > 5) {
    i = 1;
  } 
  assert(i === 1);
  return i;
}

var f = foo;
