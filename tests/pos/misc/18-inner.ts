
/*@ main :: (xx: posint) => { v: number | v > xx } */
function main(xx: number): number {

    /*@ global */
    let x_ = xx;

    let plus: (a: number) => number = function (a) {
        return a + x_;
    };

    return plus(xx);
}
