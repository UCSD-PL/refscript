
interface FooT {
  /*@ f:: /\ forall U . ((U) => U) => [U] 
          /\ forall V . ((V,V) => V) => [V] */
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

/*@ foo :: (#FooT) => [{ number | true } ] */
function foo(x) {

  var c = x.f(f1);
  var b = x.f(f2);

  return c;

}


/*@ a :: #FooT */
declare var a: FooT;

foo(a);
