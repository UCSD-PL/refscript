class IdGen<M extends ReadOnly> {
    /*@ @Mutable gen(): number */
    gen() {
        return 0;
    }

    constructor() {  }
}
class UniqueIdGen<M extends ReadOnly> extends IdGen<M> {
    private next = -1;

    /*@ @Immutable gen(): number */
    gen() {
        this.next++;
        return this.next;
    }
    constructor() {
        super();
    }
}
