getinput(){
  x := 0;
  while (?){
    x := x + 1;
  };
  return x;
}

main(){
  n := getinput ();
  if (0 < n){
    r := 1;
    i := n;
    while (0 < i){ 
      r := r + i;
      i := i - 1
    };
    assert POS : (0 < r)
  }
}
