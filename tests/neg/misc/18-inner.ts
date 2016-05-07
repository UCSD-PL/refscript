
/*@ main :: (xx: number) => { v: number | v > xx } */
function main(xx: number): number {
    /*@ readonly */
    let x = xx;
    let plus: (a: number) => number = function (a) {
        return a + x;
    };
    return plus(xx);
}
