
class AA<M extends ReadOnly> {

    /*@ (Immutable) k: number */
    public k: number;

    /*@ (Immutable) l: { number | v = this.k } */
    public l: number;

    constructor() {
        let n = random();
        this.k = n;
        this.l = n;
    }

}

let n = new AA();
assert(n.k === n.l);
