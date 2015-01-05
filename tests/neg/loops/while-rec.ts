/*@ loop :: (number) => number */
function loop(x : number) : number {
  if (x <= 4) {
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
