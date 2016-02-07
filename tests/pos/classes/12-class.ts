//adapted from navier-stokes octane
/*@ qualif Cmp  (v: int, x: a): v < len x  */


class FluidField<R extends ReadOnly> {

    /*@ (Immutable) size : posint */
    private size = 5;

    constructor() { }

    /*@ foo({ IArray<number> | len v = this.size }) : void */
    public foo(x) {
        let y = x[this.size - 1];
    }
}
