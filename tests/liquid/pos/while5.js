
// loop :: (number) => number


/*@ loop :: ({x:number|x <= 6}) => {v:number| v=6} */
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
