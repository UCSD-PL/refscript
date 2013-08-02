/*@ type list[A]  {  data : A, 
                     next : list[A] + null } */

/*@ safeNull :: forall A . (x:list[A] + null, def: list[A]) => list[A] */
function safeNull(x, def) {

  if (empty(x)) 
    return def;
  else 
    return x;

}
