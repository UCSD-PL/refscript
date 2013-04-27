/*@ random :: () -> Int */
function random(){
  var r = Math.random();
  var x = Math.floor(r * 11);
  return x;
}

/*@ pos    :: () -> {v:Int | v > 0} */
function pos(){
  var x = random();
  if (x > 0) {
    return x;
  } else {
    return (1 - x);
  }
}
