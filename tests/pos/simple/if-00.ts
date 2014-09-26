
/*@ foo :: (a: number) => { number | v = 0 } */
function foo(a: number): number {
  var i = 0;
  if (false) {
    i = 1;
  } 
  return i;
}

var f = foo;
