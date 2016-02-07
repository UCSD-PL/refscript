
class Foo {

  public bar: () => number;
  /*@ new () => { Foo<M> | 0 < 1 } */
  constructor() {
    this.bar = function() 
    /*@ <anonymous> () => number */
    { return -3; }
  }
}
