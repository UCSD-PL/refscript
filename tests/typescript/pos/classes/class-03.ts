class Foo<A { 

  public f /*@ A */ = 1;
  
  /*@ (x:A) => void */
  constructor(x) {
    this.f = x;
  }

}

var a = new Foo(1);

assert(a.f == 1);
