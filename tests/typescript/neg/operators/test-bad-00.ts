/*@ foo :: () => number */
function foo()
{
  var x = 0;
  var y = x + 1;
  assert(10 < y);
  return y
}


