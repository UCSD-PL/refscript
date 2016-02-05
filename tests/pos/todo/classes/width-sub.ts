//adapted from transducers
class Bar<M extends ReadOnly> {
  constructor() {}
}
/*@ foo :: (IArray<number>) => void */
declare function foo(a);

declare let x: Bar<ReadOnly>;

if (false) {
  foo(x);
}
