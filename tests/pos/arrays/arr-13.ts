
/*@ foo :: forall A . ( MArray<A>, A ) => { MArray<A> | 0 < 1 } */
function foo(a, e) {
  a.push(e);
  return a;
}
