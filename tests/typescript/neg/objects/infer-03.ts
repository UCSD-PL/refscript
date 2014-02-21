var gobj = {
  a: 5,
  b: "glorp",
};

/*@ foo :: () => void */
function foo() {
  gobj.a = gobj.a - 1;
  return;
}

/*@ moo :: () => {void | true} */ 
function moo(){
  foo();
  var z = gobj.a;
  assert(z > 0);
  return;
}

