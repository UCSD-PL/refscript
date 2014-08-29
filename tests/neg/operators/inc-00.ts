
/*@ inc :: ({number|true}) => void */
function inc(x){
  var y = x + 1;
  assert(y > 0);
}

