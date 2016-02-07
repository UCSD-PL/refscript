class IdGen {
  /*@ gen : (this: Self<Mutable>) : {number | 0 < 1} */
  gen() { return 0 }

  constructor() {  }
}
class UniqueIdGen extends IdGen {
  private next = -1;
  /*@ gen : (this: Self<Immutable>) : {number | 0 < 1} */
  gen() {
    this.next++;
    return this.next;
  }
  constructor() { super(); }
}
