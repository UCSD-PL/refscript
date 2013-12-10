

/*@ map :: forall A B. ((A) => B, {xs: list [A] + null | 0 <= (len xs)}) => {v: list [B] | (len v) = (len xs)} + null */
function map(f, xs){
  if (empty(xs)) {
    return nil();
  }
  return cons(f(safehead(xs)), map(f, safetail(xs)));
}

/*@ mapZ :: forall A B. ((A) => B, xs:list[A] + null) => {v:list[B] + null | (len v) = (len xs)} */
function mapZ(f, xs){
  if (empty(xs)) {
    return nil();
  } 
  return cons(f(safehead(xs)), mapZ(f, safetail(xs)));
}

