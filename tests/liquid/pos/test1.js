/*@ foo :: (int) => int */
function foo(x){
  var y = 0;
  if (0 < x) {
    y = x + y;
    assert (0 <= y);
  }
  return y;
}
