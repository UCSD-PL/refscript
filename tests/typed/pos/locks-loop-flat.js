function locks(n, flag){
  var lock = 0;
  while (n > 0){
    invariant(lock == 0);
    if (0 < flag){ lock = 1 };
    if (0 < flag){ lock = 0 };
    if (0 < flag){ lock = 1 };
    if (0 < flag){ lock = 0 };
    n = n - 1;
  }
  assert(lock == 0);
}
