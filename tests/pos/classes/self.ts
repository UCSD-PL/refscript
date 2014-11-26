class IdGen {
  /*@ gen : (this: Self<Mutable>) : {number | true} */
  gen() { return 0 }
}
class UniqueIdGen extends IdGen {
  private next = -1;
  /*@ gen : (this: Self<Mutable>) : {number | true} */
  gen() {
    this.next++;
    return this.next;
  }
}
