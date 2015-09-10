/*@ foo :: () => { number | v = 5 } */
function foo() {

  var u = 1;

  function addThree(u: number) {
    return u+3;
  }

  return addThree(u);

}

