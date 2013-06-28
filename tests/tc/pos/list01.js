
/*@ type list[A]  {  data : A, 
                     next : list[A] | Null } */



/*@ foo :: forall B . (x:list[A], A) => list[A] */
function append(x, a) {

    return { data: a , next: x };

}
