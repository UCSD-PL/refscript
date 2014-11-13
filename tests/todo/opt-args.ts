

class Foo {
  public x:number;
  /*@ new(x:number) => {void | true} */
  constructor(x=null) {
    this.x = x;
  }
}
