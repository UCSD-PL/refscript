/*@ moo :: (number) => {v:string | v = "number"} */
function moo(x) {
  var z = typeof(x);
  return z;
}
