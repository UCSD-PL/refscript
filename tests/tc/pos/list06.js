
/*@ type nlist list[number] */

/*@ next :: (x:nlist + null) => nlist + null */
function next(x) {

  if (1 > 0) 
    return x.next;
  else 
    return null;
    
}
