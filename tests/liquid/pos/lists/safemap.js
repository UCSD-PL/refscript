


/*@ map :: forall A B. ((A) => B, xs:list[A] + null) => {v:list[B] + null | (len v) = (len xs)} */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}

