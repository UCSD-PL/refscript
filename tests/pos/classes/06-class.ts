
class A<M extends ReadOnly, T> {
  constructor (x:T) { }

  /*@ n : { number | v > 0 } */
  public n : number = 5;
  /*@ m : string */
  private m : string = "a";
}

/*@ a :: A<Mutable,number> */
let a : A<Mutable, number> = new A(1);

assert(a.n > 0);
