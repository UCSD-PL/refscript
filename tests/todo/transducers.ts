
class Wrapped<T> { 
  public value: T;

  /*@ new (v: T) => void */
  constructor(v: T) {
    this.value = v;
  }

}

/*@ setFlag :: forall T . (x: T + Wrapped<Immutable, T>) => { Wrapped<Immutable,T> | true } */
function setFlag<T>(x: any): any {
  if (x instanceof Wrapped) {
    return x;
  }
  else {
    return new Wrapped(x);
  }
}

///*@ unsetFlag(x: any): T */
//function unsetFlag(x: T + Wrapped<T>): T {
//  if (x instanceof Wrapped) return x.value;
//  else return x;
//}
