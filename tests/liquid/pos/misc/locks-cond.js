/*@ qualif Locked(v:number): v != 0   */    
/*@ qualif Unlocked(v:number): v = 0  */    

// Only one of the two below is needed...

/*@ qualif CondLock1(v:number,x:number): v = ((0 < x) ? 1 : 0)  */    
/*@ qualif CondLock2(v:number,x:number): ((0 < x) <=> (v = 0))  */    

/*@ create :: () => number */
function create(){
  return 0;
}

/*@ acquire :: (number) => number */
function acquire(l){
  assert(l == 0);
  return 1;
}

/*@ release :: (number) => number */
function release(l){
  assert(l != 0);
  return 0;
}

/*@ loop :: (number, number) => number */
function loop(n, l) {
  var flag = random();
  if (0 < n){
    if (0 < flag){ 
      l = acquire(l); 
    }
    if (0 < flag){ 
      l = release(l);
    }
    loop(n-1, l);
  }
  return l;
}

/*@ main :: ({n:number|n > 0}) => void */
function main(n){
  var flag = random();
  var l    = create();
  loop(n, l);
  assert(l == 0);
}

