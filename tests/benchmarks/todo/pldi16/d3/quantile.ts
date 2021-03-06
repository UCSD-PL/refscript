// R-7 per <http://en.wikipedia.org/wiki/Quantile>
/// <reference path="include/d3.d.ts" />

/*@ mulThm1 :: (x: number, p: { real | v < 1 }) => { boolean | x * p < x } */
declare function mulThm1(x, p);

/*@ d3_quantile :: (values: {IArray<real> | 0 < len v}, p: { real | 0 <= v && v < 1 }) => real */
function d3_quantile(values: number[], p: number): number {
    let H = (values.length - 1) * p + 1;
    let h = Math.floor(H);
    let lemma1_ = mulThm1(values.length - 1, p);            // PV: adding this lemma    
    let v = +values[h - 1];
    let e = H - h;
    return e ? v + e * (values[h] - v) : v;
};

// TODO
// d3.quantile = d3_quantile;
