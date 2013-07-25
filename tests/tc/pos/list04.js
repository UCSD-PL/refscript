
/*@ type nlist  {  data : number,
                   next : nlist | Null } */

/*@ next :: (x:nlist) => nlist | Null */
function next(x) {
  return x.next;
}
