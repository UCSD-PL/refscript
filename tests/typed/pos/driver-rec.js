function random(){
  var x;
  return x;
}

function pos(){
  ensures(0 < $result);
  var x = random();
  assume(x > 0);
  return x;
}

function create(){
  ensures($result == 0);
  return 0;
}

function acquire(l){
  requires(l == 0);
  ensures($result == 1);
  assert(l==0);
  return 1;
}

function release(l){
  requires(l == 1);
  ensures($result == 0);
  assert(l==1);
  return 0;
}

function driver() {
  var newCount = pos();
  var oldCount = pos(); 
  var l        = create();
  if (newCount < oldCount){
    while (newCount != oldCount){
    invariant(((newCount != oldCount) && (l == 0)) || ((newCount == oldCount) && (l == 1)));
      l        = acquire(l);
      oldCount = newCount;
      if (0 < newCount){
        l = release(l);
        newCount = newCount - 1;
      }
    }
    l = release(l);
  }
}

