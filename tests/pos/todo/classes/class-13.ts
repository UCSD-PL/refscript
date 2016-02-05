//adapted from navier-stokes octane

class FluidField<R extends ReadOnly> {

    //this should work even if we don't label the field as immutable
    /*@ size : posint */
    private size = 5;

    constructor() { }

    /*@ foo({ IArray<number> | len v = this.size }) : void */
    public foo(x) {
        x[this.size - 1] = 0
    }
}
