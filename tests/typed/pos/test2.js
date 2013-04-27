/*@ main :: () -> int @*/
function main() {
  var y = 0;
  var x = pos();
  assert (0 <= y);
  assert (0 <= x);
  return y;
}

