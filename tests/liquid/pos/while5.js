/*@ loop :: (int) => int */
function loop(x){
  if (x <= 5) {
    var r = loop(x + 1);
    return r;
  }
  return x;
}

/*@ main :: () => void */
function main(){
  var x = loop(0);
  assert(x == 6);
}
