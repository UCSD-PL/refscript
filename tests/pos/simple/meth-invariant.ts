
/*@ qualif ArrLen(v: number, x: a) : v = (len x) */


//adapted from navier-stokes
class Foo {

    /*@ size : [Immutable] {number | v > 0} */
    private size = 5;

    constructor() { }

    /*@ bar : (x: {v: IArray<number> | (len v) = size}) : void */
    bar(x) { 

      assert(this.size - 1 >= 0);

      assert(this.size - 1 < x.length);

      x[this.size-1] = 0;

    }

}

