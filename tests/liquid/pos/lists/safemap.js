

/*@ map :: forall A B. ((A) => B, {xs: list [A] | 0 <= (len xs)}) => {v: list [B] | (len v) = (len xs)} + null */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}


