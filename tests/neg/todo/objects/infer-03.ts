
/*@ gobj :: { a: { number | v > 0 } } */
var gobj = {
  a: 5,
  b: "glorp",
};

/*@ foo :: () => void */
function foo() {
  gobj.a = gobj.a - 1;
  return;
}

/*@ moo :: () => {void | 0 < 1} */ 
function moo(){
  foo();
  var z = gobj.a;
  assert(z > 0);
  return;
}

