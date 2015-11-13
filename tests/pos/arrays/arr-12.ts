
/*@ f1 :: ( IArray<number>, MArray<number>) => { IArray<number> | 0 < 1 } */
function f1(a, e) {
  return a.concat(e);
}
