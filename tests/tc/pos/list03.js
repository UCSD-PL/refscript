/*@ type list[A]  {  data : A, 
                     next : list[A] | Null } */

/*@ append :: forall A . (x:list[A], A) => list[A] */
function append(x, a) {

    return { data: a , next: x };

}
