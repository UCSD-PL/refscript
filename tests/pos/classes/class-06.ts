
class A<T> {

  /*@ new(x:T) => void */
  constructor (x:T) {
  
  }

  /*@ n : { number | v > 0 } */
  public n : number = 5;

  /*@ m : string */
  private m : string = "a";

}

/*@ a :: #A[#Mutable,number] */
var a : A<number> = new A(1);

assert(a.n > 0);

