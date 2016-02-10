
/*@ map :: <A,B,M extends ReadOnly> ((A) => B, xs: LList<M,A>) => {v: LList<M,B> | (LLlen v) = (LLlen xs)} */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}
