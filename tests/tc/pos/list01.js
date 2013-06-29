
/*@ type nlist  {  data : number,
                   next : nlist | Null } */



/*@ append :: (x:nlist, number) => nlist */
function append(x, a) {

    return { data: a , next: x };

}
