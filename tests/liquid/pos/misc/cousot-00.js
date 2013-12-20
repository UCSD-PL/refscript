
/*@ loop :: (n:number, {m:number | m = n}) => void */
function loop(n, m){
   
  n = n + 1;
  m = m + 1;

  assert (m == n);

  return;
}
