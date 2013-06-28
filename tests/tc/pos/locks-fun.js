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
  assert(l == 1);
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

