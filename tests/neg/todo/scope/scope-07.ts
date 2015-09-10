function foo() {
  /*@ x :: number */
  var x = 2;
}
/*@ bar :: () => {number | v = 3} */
function bar() {
  if (true) {
    var x = 3;
  }
  return x;
}
