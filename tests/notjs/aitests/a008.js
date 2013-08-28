function odd(n) {
  if (n == 0) return false;
  else return even(n-1);
}

function even(n) {
  if (n == 0) return true;
  else return odd(n-1);
}

odd(17);
