
class AA<M extends ReadOnly> {

    /*@  (Immutable) k: number */
    public k: number;

    /*@ (Immutable) l: { number | v = this.k } */
    public l: number;

    constructor() {
        this.k = random();
        this.l = random();
    }

}

let n = new AA();
assert(n.k === n.l);
