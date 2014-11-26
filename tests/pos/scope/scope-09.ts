//adapted from navier-stokes
/*@ foo :: () => {void | true} */
function foo() {
  /*@ u :: string */
  var u = "hi";

  /*@ bar :: (u:number) => {void | true} */
  function bar(u) {
    u = 3;
    u = 3;
  }
  /*@ blah :: () => {void | true} */
  function blah() {
    u = u + " there"
  }
}

