getinput(){
  x := 0;
  if (?){ 
    y := 1;
  } else {
    y := 0 - 1;
  };
  while (?) {
    x := x + y ;
  };
  return x;
}

main () {
  y := 0;
  x := getinput ();
  if (0 < x) {
    y := x + y;
    assert POS: (0 <= y);
  }
}

