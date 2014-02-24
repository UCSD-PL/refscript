/*@ qualif PlusOne(v:number,x:number)   : v = x + 1    */    

/* IGNORED inc :: ({x:number|true}) => {v:number | v = x+1} */
/* IGNORED inc :: (x:number) => {v:number | v = x+1} */

/*@ inc :: (number) => number */
function inc(x){
  var res = x + 1;
  return res;
}

/*@ main :: () => void */
function main(){
  var a = pos();
  var b = inc(a);
  assert (b > (a + 1));
  assert(b > 0);
}

