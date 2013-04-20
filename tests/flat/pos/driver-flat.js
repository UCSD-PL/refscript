function driver(newCount, oldCount){
  var lock = 1;
  while (newCount != oldCount){
    lock = 1;
    oldCount = newCount;
    if (0 < newCount){
      lock = 0;
      newCount := newCount - 1;
    }
  };
  assert(lock != 0);
}
