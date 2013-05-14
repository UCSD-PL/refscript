function random(){
  var x;
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


function main(){
  var l = create();
  var x = random();
  if (0 <  x){ l = acquire(l); }
  if (0 <= x){ l = release(l); }
  assert(l == 0);
}



