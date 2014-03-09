class A {
  /*@ a :: { number | v = 1 } */
  public a : number = 1;
}

class B extends A {

}


/*@ b :: #B */
var b : B = new B();


/*@ foo :: forall A . (AA) => number */
function foo<A>(x: A) {
  return 1;
}

/* n :: { number | v = 1 } */
//var n : number = b.a;

