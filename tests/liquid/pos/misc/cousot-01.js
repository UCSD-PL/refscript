
/*@ loop :: ({n:number}, {m:number | m = n}) => void */
function loop(n, m){
   
  if (random() > 0){
    n = n + 1;
    m = m + 1;
  }

  assert (m == n);

  return;
}
