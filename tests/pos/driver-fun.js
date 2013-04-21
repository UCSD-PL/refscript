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

function driver(l0, newValue0, oldValue0){
  var l;
  var newValue;
  var oldValue;
  if (newValue0 != oldValue0){
    l        = lock(l0);
    oldValue = newValue0;
    if (0 < newValue0){
      l = unlock(l);
      newValue = newValue0 - 1;
    } else {
      newValue = newValue0;
    }
    l = driver(l, newValue, oldValue);
  };
  return l;

}

function main() {
  var newValue = pos();
  var oldValue = pos(); 
  var l        = create();
  if (newValue < oldValue) {
    l = driver(l, newValue, oldValue); 
    l = unlock(l);
  }
}

