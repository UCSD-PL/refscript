/*@ foo :: ({ IArray<number> | (len v) <= 10 } ) => number */
function foo(a:number []) : number {
  return a[9];
}