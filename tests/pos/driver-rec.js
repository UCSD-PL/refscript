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

function driver() {
  var newValue = pos();
  var oldValue = pos(); 
  var l        = create();
  if (newValue < oldValue){
    while (newValue != oldValue){
      l        = lock(l);
      oldValue = newValue;
      if (0 < newValue){
        l = unlock(l);
        newValue = newValue - 1;
      }
    }
    l = unlock(l);
  }
}

