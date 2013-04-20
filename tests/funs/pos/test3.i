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

getposinput(){
  x := 0;
  while (?){
    x := x + 1;
  }; 
  return x;
}


main(){
  y := 0;
  x := getposinput();
  y := x + y;
  assert POS: (0 <= y);
}
