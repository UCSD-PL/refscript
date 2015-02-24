
/*@ foo :: forall A . ( MArray<A>, A ) => { MArray<A> | true } */
function foo(a, e) {
  a.push(e);
  return a;
}
