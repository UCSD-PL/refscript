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

newlock(){
  return 0;
}

lock(l){
  assert UNLOCKED: (l = 0);
  return 1;
}

unlock(l){
  assert LOCKED: (l = 1);
  return 0;
}

main(){
  l := newlock();
  n := getinput();
  x := getinput();

  while (0 < n){
    if (0 < x){ l := lock(l) };
    if (0 <= x){ l := unlock(l) };
    n := n - 1
  };
  assert NOLOCK : (l = 0)
}
