
/*@ qualif Add(v: number, n: number, m: number): v = m + n */
/*  qualif Mul(v: number, n: number, m: number): v = mul m n */


/// <reference path="mulThms-1.d.ts" />

class FluidField {
    /*@ width : [Immutable] pos */
    private width;
    /*@ height : [Immutable] pos */
    private height;
    /*@ rowSize : [Immutable] {v:number | v = this.width + 2} */
    private rowSize;
    /*@ size : [Immutable] {v:number | v = (mul (this.height+2) (this.width+2)) } */
    private size;

    /*@ new (hRes:pos,
             wRes:{v:pos | (mul hRes v) < 1000000}) => {v:FluidField<M> | offset(v,"width") = wRes && offset(v,"height") = hRes} */
    constructor(hRes, wRes) {
        var width = wRes;
        var height = hRes;
        var size = multiply(height + 2, width + 2);

        this.width = width;
        this.height = height;
        this.rowSize = width + 2;
        this.size = size;
    }

    /*@ lin_solve : (x :{v:IArray<number> | (len v) = this.size}) : void */
    lin_solve(x: number[]) {
        var width = this.width;
        var height = this.height;
        var rowSize = this.rowSize;

        var currentRow = multiply(height, rowSize);

        var _lemma2 = mulThm2(rowSize, height  , height + 2);

        // assume(multiply(height, width + 2) + (width + 2) === multiply(height + 1, width + 2));
        // assume(multiply(height + 1, width + 2) < multiply(height + 2, width + 2));

        ++currentRow;
        for (var i = 0; i < width; i++) {
            var q = x[currentRow];
            ++currentRow;
        }
    }
}
