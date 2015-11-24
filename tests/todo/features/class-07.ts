

class A<T> {

  constructor () { }

  /*@ ago: (x:T): void */
  public ago(x:T) { }

}

class B<S,R> extends A<R> {
  
  /*@ bgo : (x:S, y:R ): { void | 0 < 1 } */
  public bgo(x:S, y:R) {
    super.ago(y);
  }

  constructor() { super(); }
}

class C <M,L,K> extends B<M,K> {

  /*@ cgo : (x:M, y:L, z:K): { void | 0 < 1 } */
  public cgo(x:M, y:L, z:K) {
    super.bgo(x,z);
  }

  constructor() { super(); }

}

