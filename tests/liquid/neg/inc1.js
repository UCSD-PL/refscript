
/*@ inc :: (int) => void */
function inc(x){
  assume(x > 0);
  var y = x + 1;
  assert(y > 0);
  return 1;
}

