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
  new := getinput();
  old := getinput();
  while ~(new = old){
    l := lock(l);
    old := new;
    if (0 < new){
      l := unlock(l);
    }
  };
  l := unlock(l);
}
