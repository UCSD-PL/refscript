function locks(x){
  var lock = 0;
  if (0 < x){ 
    lock = 1;
  };
  
  if (0 < x){ 
    lock = 0; 
  };

  assert(lock == 0);
}
