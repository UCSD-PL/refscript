

class A<T> {

  constructor () { }

  /*@ ago: (x:T): void */
  public ago(x:T) { }

}

class B<S,R> extends A<R> {
  
  /*@ bgo : (x:S, y:R ): { void | true } */
  public bgo(x:S, y:R) {
    super.ago(y);
  }

  constructor() { super(); }
}

class C <M,L,K> extends B<M,K> {

  /*@ cgo : (x:M, y:L, z:K): { void | true } */
  public cgo(x:M, y:L, z:K) {
    super.bgo(x,z);
  }

  constructor() { super(); }

}

