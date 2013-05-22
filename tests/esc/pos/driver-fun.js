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
  return 1;
}

function release(l){
  requires(l == 1);
  ensures($result == 0);
  return 0;
}

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

function main() {
  var newCount = pos();
  var oldCount = pos(); 
  var l        = create();
  if (newCount < oldCount) {
    l = driver(l, newCount, oldCount); 
    l = release(l);
  }
}

