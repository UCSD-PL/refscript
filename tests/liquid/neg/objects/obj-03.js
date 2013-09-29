
var x = {
  a: 5,
  b: "String",
}

/*@ foo :: () => { v: number | v = 4 } */
function foo () {
  
  var y = { a: 4 }
  
  y = x;

  return y.a;

}
