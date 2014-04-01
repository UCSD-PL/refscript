/*@ foo :: ({ [number] | (len v) = 1 } ) => number */
function foo(a : number []) : number {
  return a[0];
}
