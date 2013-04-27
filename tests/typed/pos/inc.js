/*@ inc :: (int) -> int @*/
function inc(x){
  requires(x > 0);
  ensures($result > 0);
  ensures($result == x + 1);
  var res = x + 1;
  return res;
}

/*@ main () -> Void @*/
function main(){
  var a = pos();
  var b = inc(a);
  assert (b == a + 1);
}

