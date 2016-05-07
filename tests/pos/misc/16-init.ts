
/*@ init :: (number) => {v: number | v > 0} */
function init(n: number): number {
    /*@ local i :: number + undefined */
    let i: number;
    if (n > 0) {
        i = 1;
    } else {
        i = 2;
    }
    return i;
}
