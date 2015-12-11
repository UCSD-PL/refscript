/*@ foo :: (o: { [x:string]: number }) => {[x:{string | v > "cat" && hasProperty(v, o)}]: number}
 */
function foo(o) {
  var res = {};
  for (var k in o)
    if (k > "cat")
      res[k] = o[k]
  return res;
}
