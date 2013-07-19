/*@ type nlist  { data : number, 
                  next : nlist } */

/*@ append :: (x:nlist, {v:number| 1 > 0}) => nlist */
function append(x, a) {

    return { data: a , next: x };

}


/* append1 :: (x:nlist, number) => nlist */
//function append1(x, a) {

//    return { data: a , next: x };

//}

/* append1 :: (x:nlist | Null, number) => nlist */
// function append1(x, a) {
// 
//     return { data: a , next: x };
// }

