
/*@ type nlist  {  data : number,
                   next : nlist + null } */

/*@ next :: (x:nlist) => nlist */
function next(x) {
  return x.next;
}
