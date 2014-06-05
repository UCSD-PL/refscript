
interface FooT {
  /*@ f:: /\ forall U . ((U) => U) => #Array[#Immutable,U] 
          /\ forall V . ((V,V) => V) => #Array[#Immutable,V] */
  f: (x:any) => any;
}

/*@ f1 :: (number) => number */
function f1(x:number):number {
  return x;
}

/*@ f2 :: (number, number) => number */
function f2(x:number, y:number):number {
  return x;
}

/*@ foo :: (#FooT[#Immutable]) => #Array[#Immutable, number] */
function foo(x) {

  var c = x.f(f1);
  var b = x.f(f2);

  return c;

}


/*@ a :: #FooT[#Immutable] */
declare var a: FooT;

foo(a);
