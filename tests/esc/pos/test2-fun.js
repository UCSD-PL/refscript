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

function main() {
  var y = 0;
  var x = pos();
  assert (0 <= y);
}

