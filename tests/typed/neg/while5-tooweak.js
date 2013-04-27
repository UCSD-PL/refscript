function foo(){
  var x = 0;
  while (x <= 5){
    invariant(x <= 7);
    x = x + 1;
  }
  assert(x == 6);
}
