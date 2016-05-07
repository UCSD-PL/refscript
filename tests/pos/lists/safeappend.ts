 /*@ append :: <A, M extends ReadOnly> (xs: LList<M, A>, ys: LList<M, A>) => {v : LList<M, A> | (LLlen v) = (LLlen xs) + (LLlen ys)} */
function append(xs, ys){
  if (empty(xs)) {
    return ys;
  } else {
    let x   = safehead(xs);
    let xs_ = safetail(xs);
    return cons(x, append(xs_, ys));
  }
}
