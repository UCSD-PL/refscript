

class A<T> {

  /*@ constructor :: (x:T) => void */
  constructor (x:T) { }


  /*@ ago :: (x:T)=>void */
  public ago(x:T) { }

}

class B<S,R> extends A<R> {
  
  /*@ bgo :: (x:S, y:R )=> void */
  public bgo(x:S, y:R) {
    super.ago(y);
  }
}

class C <M,L,K> extends B<M,K> {

  /*@ cgo :: (x:M, y:L, z:K )=> void */
  public cgo(x:M, y:L, z:K) {
    super.bgo(x,z);
  }

}


