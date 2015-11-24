/*@ boop :: (#List [number]) => {v:boolean | 0 < 1 } */
function boop(xs : number[]) : boolean{
  return emptyPoly(xs)
}
