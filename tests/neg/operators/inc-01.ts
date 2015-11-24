/*@ qualif PlusOne(v:number,x:number)   : v = x + 1    */    

/* IGNORED inc :: ({x:number|0 < 1}) => {v:number | v = x+1} */
/* IGNORED inc :: (x:number) => {v:number | v = x+1} */

/*@ inc :: (number) => number */
function inc(x){
  let res = x + 1;
  return res;
}

/*@ main :: () => void */
function main(){
  let a = pos();
  let b = inc(a);
  assert (b > (a + 1));
  assert(b > 0);
}

