

/*@ map :: forall A B. ((A) => B, xs: { [A] | 0 <= (len v)}) => { [B] | (len v) = (len xs) } */
function map(f, xs) {
  
  if (xs.length == 0) {
    return [];
  }

  //var x = xs.pop();
  //var y = f(x);
 
  //var ys = map(f,xs);
  
  //ys.push(y); 
  
  //return ys;
  
  return [];

}


