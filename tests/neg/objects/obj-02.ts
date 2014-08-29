
var x = {
  moo: 5,
  b: "String",
};

/*@ foo :: () => { v: number | v = 5 } */
function foo () {
  var y = x;
  x.moo = 4345;
  return y.moo;

}
