
/*@ revInc :: forall M . (a: IArray<number>) 
           => { IArray<number> | (len v) = (len a) } */
function revInc(a: number[]) {

  a.reverse();

  for (var i = 0; i < a.length; i++) {
    a[i] = a[i] + 1; 
  }
  a.push(1);

  return a;
}
