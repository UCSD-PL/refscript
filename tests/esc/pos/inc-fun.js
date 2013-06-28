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

/*@ inc :: (number) => number @*/
function inc(x){
  ensures($result == x + 1);
  var res = x + 1;
  return res;
}

/*@ main :: () => void @*/
function main(){
  var a = pos();
  var b = inc(a);
  assert (b == a + 1);
}

