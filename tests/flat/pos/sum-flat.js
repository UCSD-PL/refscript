function sum(n){
  assume(0 < n);
  var r = 1;
  var i = n;
  while (0 < i) { 
    r = r + 1;
    i = i - 1;
  }
  assert (0 <  r);
  assert (n == r);
}
