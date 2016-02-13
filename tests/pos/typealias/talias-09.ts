/*@ type eq[s] = {number | v = s} */

class Foo<M extends ReadOnly> {

    /*@ (Immutable) x: number */
    private x = 3;

    /*@ (Immutable) y: eq<this.x> */
    private y = 3;

    constructor() { }
}
