
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
  var x = random();
  var l = create();
  if (0 <  x){ l = lock(l); }
  if (0 <= x){ l = unlock(l); }
  assert(l == 0);
}



