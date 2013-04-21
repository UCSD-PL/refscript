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
  assert(l == 0);
  return 1;
}

function release(l){
  requires(l == 1);
  ensures($result == 0);
  assert(l == 1);
  return 0;
}

function main(){
  var n    = pos();
  var flag = random();
  var l    = create();

  while (0 < n){
    if (0 < flag){ 
      l = lock(l) 
    }
    if (0 <= flag){ 
      l = unlock(l) 
    }
    n = n - 1;
  }
  assert(l == 0);
}

