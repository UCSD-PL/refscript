x = 0;
while (x <= 5){
  invariant(x <= 6);
  x = x + 1;
}
assert(x == 6);
