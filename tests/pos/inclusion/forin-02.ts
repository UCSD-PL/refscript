// Taken from strobe
/*@ qualif HasP(v:string, s:A): hasProperty(v, s) */
/*@ qualif EnumP(v:string, s:A): enumProp(v,s)    */

/*@ foo :: (o: { [x:string]: number }) => {[x:{string | v == "cat" && hasProperty(v, o)}]: number}
 */
function foo(o) {
  var res = {};
  for (var k in o)
    if (k == "cat")
      res[k] = o[k]
  return res;
}
