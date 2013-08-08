/*@ type nlist  list[number] */

/*@ main :: (x:nlist, v:number) => nlist */
function main(x, a) {
    return { data: a , next: x };
}

/*@ append1 :: (x:nlist + null, number) => nlist */
function append1(x, a) {
  return { data: a , next: x };
}

