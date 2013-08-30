
/*@ foo :: () => {string | v = "bar" } */
function foo () {
  var x = {foo: "bar"};
  x.moo = "cow";
  return x.foo;
}
