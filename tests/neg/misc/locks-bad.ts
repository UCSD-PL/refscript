/*@ qualif Locked(v:number): v != 0    */    
/*@ qualif Unlocked(v:number): v = 0  */    

/*@ create :: () => number */
function create(){
  return 0;
}

/*@ acquire :: (number) => number */
function acquire(l){
  assert(l === 0);
  return 1;
}

/*@ release :: (number) => number */
function release(l){
  assert(l != 0);
  return 0;
}

/*@ work :: () => void */
function work(){
  return;
}

/*@ loop :: (number, number) => number */
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


/*@ main :: ({n:number|n > 0}) => void */
function main(n){
  var l = create();
  l = loop(n, l);
  assert(l === 0);
}

