
/*@ inc :: (x:{v:int | v > 0}) -> {v:int | (v > 0)} @*/
function inc(x){
  assume(x > 0);
  y = x + 1;
  assert(y > 0);
}

