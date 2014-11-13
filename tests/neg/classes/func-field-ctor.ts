
class Foo {

  /*@ bar : () => { number | v > 0 } */
  public bar: () => number;
  /*@ new () => { void | true } */
  constructor() {
    this.bar = function() 
    /*@ <anonymous> () => number */
    { return -3; }
  }
}
