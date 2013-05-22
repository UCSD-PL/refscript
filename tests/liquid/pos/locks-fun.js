/*@ qualif Locked(v:int): v = 1    */    
/*@ qualif Unlocked(v:int): v = 0  */    
/* qualif CondLock(v:int,x:int): v = ((0 < x) ? 0 : 1)  */    

/*@ create :: () => int */
function create(){
  return 0;
}

/*@ acquire :: (int) => int */
function acquire(l){
  assert(l == 0);
  return 1;
}

/*@ release :: (int) => int */
function release(l){
  assert(l == 1);
  return 0;
}

/*@ loop :: (int, int) => int */
function loop(n, l) {
  
  var flag = random();
  assert(0 == 1);

  if (0 < n){
    if (0 < flag){ 
      l = acquire(l); 
    }
    if (0 < flag){ 
      l = release(l);
    }
    n = n - 1;
    loop(n-1, l);
  }
  return l;
}

/*@ main :: ({n:int|n > 0}) => void */
function main(n){
  var flag = random();
  assert (0 == 1);
  var l    = create();
  loop(n, l);
  assert(l == 0);
}

