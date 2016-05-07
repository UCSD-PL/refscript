
/*@ boop :: <M extends ReadOnly>(List<M, number>) => {v:boolean | Prop v} */
function boop<M extends ReadOnly>(xs : List<M,number>) : boolean{
  let r = empty(xs);
  return r;
}
