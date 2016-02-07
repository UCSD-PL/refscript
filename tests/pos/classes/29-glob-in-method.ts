class Blah {
  /*@ new () => Blah<M> */
  constructor() { }

  public state = 3;

  /*@ foo : () : { void | 0 < 1 } */
  public foo() {
    /*@ val :: number + null */
    var val = null;
    if (this.state === 42) val = 7;
  }
}

