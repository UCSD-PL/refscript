
class FooT {
  /*@ fff :: /\ forall U . ((U) => U  ): #Array[#Immutable,U] 
             /\ forall V . ((V,V) => V): #Array[#Immutable,V] */
  public fff(x:any): any {
    return [];
  }
}

/*@ f1 :: (number) => number */
function f1(x:number):number {
  return x;
}

/*@ f2 :: ({ number | v > 0 } , number) => { number | v > 0 } */
function f2(x:number, y:number):number {
  return x;
}

/*@ foo :: (#FooT[#Immutable]) => #Array[#Immutable, number] */
function foo(x) {

  var c = x.fff(f1);
  var b = x.fff(f2);

  return c;

}


/*@ a :: #FooT[#Immutable] */
declare var a: FooT;

foo(a);
