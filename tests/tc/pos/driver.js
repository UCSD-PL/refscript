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

/*@ driver :: (int, int, int) => int */ 
function driver(l0, newCount0, oldCount0){
  requires(((newCount0 != oldCount0) && (l0 == 0)) || ((newCount0 == oldCount0) && (l0 == 1)));
  ensures($result == 1);
  var l        = l0;
  var newCount = newCount0;
  var oldCount = oldCount0;
  
  if (newCount != oldCount){
    l        = acquire(l0);
    oldCount = newCount0;
    if (0 < newCount){
      l = release(l);
      newCount = newCount - 1;
    } else {
      newCount = newCount;
    }
    l = driver(l, newCount, oldCount);
  };
  return l;
}

/*@ main :: () => void */
function main() {
  var newCount = pos();
  var oldCount = pos(); 
  var l        = create();
  if (newCount < oldCount) {
    l = driver(l, newCount, oldCount); 
    l = release(l);
  }
}

