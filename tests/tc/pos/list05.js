/*@ type list[A]  {  data : A, 
                     next : list[A] + Null } */

/*@ safeNull :: forall A . (x:list[A] + Null, def: list[A]) => list[A] */
function safeNull(x, def) {

  if (x != null) 
    return x;
  else 
    return def;

}
