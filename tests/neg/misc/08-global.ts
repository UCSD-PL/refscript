
/*@ foo :: ({v:number|v>0}) => {v:number|v<0} */ 
function foo(a) {

  /*@ bar :: () => void */
  function bar() {
    a = -a;
  }
  bar();

  return a;
}
