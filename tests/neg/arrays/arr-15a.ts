
/*@ revInc :: (a: IArray<number>) => { IArray<number> | len v = len a } */
export function revInc(a: number[]) {
  a.reverse();
  for (let i = 0; i < a.length; i++) {
    a[i] = a[i] + 1;
  }
  a.push(1);
  return a;
}
