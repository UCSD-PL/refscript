/*@ foo :: (a: { IArray<number> | len v <= 10 } ) => number */
export function foo(a:number []) : number {
  return a[9];
}
