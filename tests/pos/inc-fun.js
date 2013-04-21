function pos(){
  ensures($result > 0);
  var x;
  assume (x > 0);
  return x;
}

function inc(x){
  requires(x > 0);
  ensures($result > 0);
  ensures($result == x + 1);
  var res = x + 1;
  return res;
}

function main(){
  var a = pos();
  var b = inc(a);
  assert (b == a + 1);
}

