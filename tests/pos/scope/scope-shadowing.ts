

/*@ foo :: () => {number | v = 4 } */
function foo() {

  var u = 1;

  function addThree(u: number) {
    return u+3;
  }

  return addThree(u);

}



var w = 1;

class ABC {

  /* addThree : (u: number): { number | v = u + 3 }*/
  public addThree(u: number) {
    return u+3;
  }

}

var a = new ABC();

assert(a.addThree(w) === 4);
