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

function sumLoop(acc, i){
  requires(0 <= i);
  ensures($result == acc + i);
  var r = 0;
  if (0 < i){
    r = sumLoop(acc + 1, i - 1);
  } else {
    r = acc;
  }
  return r;
}

function sum(){
  var n = pos();
  var m = sumLoop(0, n);
  assert(m == n);
}
