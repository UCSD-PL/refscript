function sum(n){
  assume(n > 0);
  var r = 0;
  var i = n;
  while (0 < i) {
    invariant(i >= 0);
    invariant((i + r) == n);
    invariant(n > 0);
    r = r + 1;
    i = i - 1;
  }
  assert (i == 0);
  assert (n == r);
  assert (0 <  r);
}
