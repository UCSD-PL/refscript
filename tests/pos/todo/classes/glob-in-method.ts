class Blah {
  /*@ new () => Blah<M> */
  constructor() { }

  public state = 3;

  /*@ foo : () : { void | true } */
  public foo() {
    /*@ val :: number + null */
    var val = null;
    if (this.state === 42) val = 7;
  }
}

