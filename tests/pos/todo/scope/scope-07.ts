function foo() {
  /*@ x :: number */
  var x = 2;
}
function bar() {

  if (true) {
    /*@ x :: number */
    var x = 3;

  }

}
