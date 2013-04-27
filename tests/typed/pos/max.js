/*@ max :: (int, int) => int @*/ 
function max(x, y){ 
  var r = 0;
  if (x > y) {
    r = x;
  } else {
    r = y;
  }
  assert(r >= x);
  assert(r >= y);
  return r;
}
