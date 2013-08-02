
/*@ type nlist  {  data : number,
                   next : nlist + null } */

/*@ next :: (x:nlist) => nlist + null */
function next(x) {
  return x.next;
}
