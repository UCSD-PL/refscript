
/*@ revInc :: forall M . (Array<Mutable,number>) 
           => { IArray<number> | 0 < 1 } */
function revInc(a: number[]) {

  a.reverse();

  for (var i = 0; i < a.length; i++) {
    a[i] = a[i] + 1; 
  }
  
  return a;
}
