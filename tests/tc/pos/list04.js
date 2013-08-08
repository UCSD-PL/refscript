
/*@ type nlist  list[number] */ 

/*@ next :: (x:nlist) => nlist + null */
function next(x) {
  return x.next;
}
