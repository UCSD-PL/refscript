
/*@ loop :: (int, int) => int @*/
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

/*@ main :: () => void @*/
function main(){
  var n    = pos();
  var flag = random();
  var l    = create();
  loop(n, l);
  assert(l == 0);
}

