foo(){
  if (?) { 
    return 0;
  } else {
    return 1;
  }
}

main(){
  new := foo();
  old := foo();
  assert BAR: (old = 1) ;
}
