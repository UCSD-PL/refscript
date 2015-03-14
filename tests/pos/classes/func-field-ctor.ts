
class Foo {

  public bar: () => number;
  /*@ new () => { Foo<M> | true } */
  constructor() {
    this.bar = function() 
    /*@ <anonymous> () => number */
    { return -3; }
  }
}
