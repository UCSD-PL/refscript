/*@ qualif Locked(v:int): v != 0    */    
/*@ qualif Unlocked(v:int): v = 0  */    

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
  assert(l != 0);
  return 0;
}

/*@ work :: () => void */
function work(){
  return;
}

/*@ loop :: (int, int) => int */
function loop(n, l) {
  
  var flag = random();
  if (n <= 0) {
    return l;
  }

  l = acquire(l); 
  work();
  // l = release(l);
  
  return loop(n-1, l);
}


/*@ main :: ({n:int|n > 0}) => void */
function main(n){
  var l = create();
  loop(n, l);
  assert(l == 0);
}

