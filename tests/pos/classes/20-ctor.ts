
class AA<M extends ReadOnly> {
    /*@ (Immutable) x : number */
    public x = 0;

    /*@ (Immutable) y: { number | v = this.x } */
    public y = 0;

    constructor() { }
}

// let aa = new AA();
// assert(aa.x === aa.y);
