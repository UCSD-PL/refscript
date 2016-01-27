
/*@ qualif ArrLen(v: int, x: a) : v = len x */

//adapted from navier-stokes
export class Foo<M extends ReadOnly> {

    /*@ @Final size: {number | v > 0} */
    size = 5;

    constructor() { }

    /*@ bar(x: { v: IArray<number> | len v = this.size }) : void */
    bar(x) {
        assert(this.size - 1 >= 0);
        assert(this.size - 1 < x.length);
        x[this.size - 1] = 0;
    }

}
