/*@ type nlist  { data : number, 
                  next : nlist } */

/*@ type blist  { data : boolean, 
                  next : blist } */

/*@ append :: (x:nlist, number) => blist */
function append(x, a) {
  return { data: a , next: x };
}

