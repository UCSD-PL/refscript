/*@ assert :: (bool) => void */ 
function assume(p){
  return;
}

/*@ assert :: (bool) => void */ 
function assert(p){
  return;
}

/*@ random :: () => int */
function random(){
  var r = Math.random();
  var x = Math.floor(r * 11);
  return x;
}

// pos    :: () -> {v:Int | v > 0}

/*@ pos    :: () => int */
function pos(){
  var x = random();
  if (x > 0) {
    return x;
  } else {
    return (1 - x);
  }
}
