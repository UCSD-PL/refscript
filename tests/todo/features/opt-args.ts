

class Foo {
  public x:number;
  /*@ new(x:number) => {void | 0 < 1} */
  constructor(x=null) {
    this.x = x;
  }
}
