/*@ alias nat = {number | 0 <= v } */

/*@ ab :: (number) => nat */
function ab(x){
  if (x > 0){
    return x;
  }
  return (0 - x);
}
