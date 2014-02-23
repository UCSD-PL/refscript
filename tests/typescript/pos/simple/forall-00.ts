
/*@  type fooT = { f: /\ forall U . ((U) => U) => [U] 
                      /\ forall V . ((V,V) => V) => [V] } */

/*@ f1 :: (number) => number */
function f1(x:number):number {
  return x;
}

/*@ f2 :: (number, number) => number */
function f2(x:number, y:number):number {
  return x;
}

/*@ foo :: (fooT) => void */
function foo(x):void {

  x.f(f1);
  x.f(f2);

}
