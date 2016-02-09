//adapted from navier-stokes octane

class FluidField<R extends ReadOnly> {

    //this should work even if we don't label the field as immutable

    /*@ size: {number | v > 0} */
    private size = 5;

    constructor() { }

    /*@ foo(x: { IArray<posint> | len v = this.size }) : void */
    public foo(x) {
        x[this.size] = -1
    }
}
