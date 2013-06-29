
/*@ type nlist  {  data : number,
                   next : nlist | Null } */

/*@ append :: (x:nlist) => nlist | Null */
function tail(x) {
  return x.next;
}
