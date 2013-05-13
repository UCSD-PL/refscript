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

/*@ main :: () => void */
function main(){
  var n    = pos();
  var flag = random();
  var l    = create();
  loop(n, l);
  assert(l == 0);
}

