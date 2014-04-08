/*@ alias nat = { number | 0 <= v } */

/*@ ab :: (number) => nat */
function ab(x:number): number {
  if (x > 0){
    return x;
  }
  return (0 - x);
}
