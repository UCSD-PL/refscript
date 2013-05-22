function random(){
  var x;
  return x;
}

function main() {
  var y = 0;
  var x = random();
  if (0 < x) {
    y = x + y;
    assert (0 <= y);
  }
}

