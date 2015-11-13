
interface FooT {
  /*@ f: /\ forall M U . ((U) => U) => #Array[M,U] 
          /\ forall M V . ((V,V) => V) => #Array[M,V] */
  f: (x:any) => any;
}

/*@ f1 :: (number) => number */
function f1(x:number):number {
  return x;
}

/*@ f2 :: (number, string) => number */
function f2(x:number, y: string): number {
  return x;
}

/*@ foo :: forall M . (#FooT) => #Array[M, { number | 0 < 1 } ] */
function foo(x) {

  var c = x.f(f1);
  var b = x.f(f2);

  return c;

}


/*@ a :: #FooT[#Immutable] */
declare var a: FooT;

foo(a);
