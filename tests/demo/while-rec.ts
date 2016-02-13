/*@ loop :: ({x:number|x <= 6}) => {v:number| v = 6} */
function loop(x) {
  if (x <= 5) {
    var r = loop(x + 1);
    return r;
  }
  return x;
}

/*@ main :: () => void */
function main() : void {
  var x = loop(0);
  assert(x === 6);
}
