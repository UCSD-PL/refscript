
/*@ revInc :: <M>(a: IArray<number>) => { IArray<number> | len v = len a } */
export function revInc(a: number[]) {
  // a.reverse();       // currently not allowed on non-mutable arrays
  for (let i = 0; i < a.length; i++) {
    let b = a[i] + 1;
  }
  return a;
}
