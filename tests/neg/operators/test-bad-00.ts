/*@ foo :: () => number */
function foo()
{
  let x = 0;
  let y = x + 1;
  assert(10 < y);
  return y
}


