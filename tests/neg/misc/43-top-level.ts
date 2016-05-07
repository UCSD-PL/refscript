let a = 1;

module A {
    /*@ foo :: () => { number | v < 0 } */
    function foo() {
      return a ;
    }
}
