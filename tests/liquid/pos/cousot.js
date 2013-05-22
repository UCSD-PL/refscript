// DEMO EXAMPLE. Crucially requires:

/*@ qualif Equal(v:int, x:int) : v = x */

// Note that the example works even "without" the above, 
// as this qualifier is in include/prelude.js

/*@ loop :: (int, int) => void */

function loop(n, m){
  
  if (n == 0) {
    assert (m == 0);
    return;
  }
  
  if (random() > 0){
    n = n + 1;
    m = m + 1;
  } 

  if (random() > 0){
    n = n - 1;
    m = m - 1;
  }
  
  loop(n, m);
}

/*@ main :: ({n:int| n > 0}) => void */
function main(n){
   loop(n, n); 
}
