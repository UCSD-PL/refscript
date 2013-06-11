
// loop :: (int) => int


/*@ loop :: ({x:int|x <= 6}) => {v:int| v=6} */
function loop(x){
  if (x <= 5) {
    return loop(x + 1);
  }
  return x;
}

/*@ main :: () => void */
function main(){
  var x = loop(0);
  assert(x == 6);
}
