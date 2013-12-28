var glob /*@ { number | v > 0 } */ = 4;

/*@ bar :: () => {void | true} */
function bar(){
  glob = 7; 
  return;
}

/*@ zoo :: () => {void | true} */
function zoo(){
  bar();
  assert(glob > 0);
}
