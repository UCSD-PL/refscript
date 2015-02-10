class IdGen {
  /*@ gen : (this: Self<Mutable>) : {number | true} */
  gen() { return 0 }

  constructor() {  }
}
class UniqueIdGen extends IdGen {
  private next = -1;
  /*@ gen : (this: Self<Immutable>) : {number | true} */
  gen() {
    this.next++;
    return this.next;
  }
  constructor() { super(); }
}
