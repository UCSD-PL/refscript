/*@ type list[A]  {  data : A, 
                     next : list[A] + Null } */


/*@ append :: (x:list[number], number) => list[number] */
function append(x, a) {

    return { data: a , next: x };

}
