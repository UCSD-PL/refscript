
/*@ type Nat = {v: number | 10 <= v } */

/*@ ab :: (number) => Nat */
export function ab(x:number): number {
  if (x > 0){
    return x;
  }
  return (0 - x);
}
