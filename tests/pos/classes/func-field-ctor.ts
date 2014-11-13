
class Foo {

  public bar: () => number;
  /*@ new () => { void | true } */
  constructor() {
    this.bar = function() 
    /*@ <anonymous> () => number */
    { return -3; }
  }
}
