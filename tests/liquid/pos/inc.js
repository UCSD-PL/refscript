
/* qualif PlusOne(v:int,x:int)   : v = x + 1    */    

// IGNORED inc :: ({x:int | x > 0}) => {v:int | v = x + 1}


/*@ inc :: (int) => int */
function inc(x){
  var res = x + 1;
  return res;
}

/*@ main :: () => void */
function main(){
  var a = pos();
  var b = inc(a);
  var c = (b != (a + 1));
  assert(c);
  assert(b > 0);
}

