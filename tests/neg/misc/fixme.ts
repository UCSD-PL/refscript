
/*@ work :: () => void */
function work(){
  return;
}

/*@ bar :: () => {v:number | v = 0} */
function bar(){
  work();
  return 6;
}
