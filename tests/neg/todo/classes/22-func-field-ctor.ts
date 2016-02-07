
class Foo {

  /*@ bar : () => { number | v > 0 } */
  public bar: () => number;
  /*@ new () => { void | 0 < 1 } */
  constructor() {
    this.bar = function() 
    /*@ <anonymous> () => number */
    { return -3; }
  }
}
