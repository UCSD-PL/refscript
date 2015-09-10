

/*@ foo :: forall T . (x: Array<Immutable,T>) => T + { undefined | true } */
function foo<T>(x: T[]): T {

  /*@ b :: T + undefined */
  var b: T;

  if (x.length > 0) b = x[0];

  return b;

}
