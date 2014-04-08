


/*@ map :: forall A B. ((A) => B, xs: #List[A]?) => {v: #List[B]? | (len v) = (len xs)} */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}

