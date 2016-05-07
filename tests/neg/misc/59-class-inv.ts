
/*@ inv (this.rowSize = this.width + 2) */
class FluidField<M extends ReadOnly> {
    /*@ (Immutable) width : number */
    private width;
    /*@ (Immutable) rowSize : number */
    private rowSize;

    /*@ new () : {v:FluidField<M> | 0 < 1} */
    constructor() {
        let width = 3;

        this.width = width;
        this.rowSize = width + 1;
    }
}
