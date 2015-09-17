
/*@ abs :: (x: number) => {res: number | res >= 0} */
export function abs(x: number): number {
    var r = x;
    if (x > 0) {
        r = x;
    }
    else {
        r = (0 - x);
    }
    return r;
}
