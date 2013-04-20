getposinput(){
  x := 0;
  while (?){
    x := x + 1;
  }; 
  return x;
}

fib(y){
  if (y <= 1) {
    return 1;
  };
  y1 := fib(y - 1);
  y2 := fib(y - 2);
  return y1 + y2;
}

main(){
  x := getposinput();
  r := fib(x);
  assert POS: (0 < r);
}
