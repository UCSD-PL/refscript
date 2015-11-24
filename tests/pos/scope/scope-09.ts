//adapted from navier-stokes
/*@ foo :: () => {void | 0 < 1} */
function foo() {
  /*@ u :: string */
  var u = "hi";

  /*@ bar :: (u:number) => {void | 0 < 1} */
  function bar(u) {
    u = 3;
    u = 3;
  }
  /*@ blah :: () => {void | 0 < 1} */
  function blah() {
    u = u + " there"
  }
}

