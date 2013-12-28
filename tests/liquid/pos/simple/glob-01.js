var glob /*@ { number | v > 0 } */ = 12;

/*@ bar :: () => {void | true} */
function bar(){
  glob = 7; 
  return;
}

