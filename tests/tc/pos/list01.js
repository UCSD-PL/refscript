/*@ type nlist  { data : number, next : nlist + Null} */

/*@ main :: (x:nlist, v:number) => nlist */
function main(x, a) {
    return { data: a , next: x };
}

/*@ append1 :: (x:nlist + Null, number) => nlist */
function append1(x, a) {
  return { data: a , next: x };
}

