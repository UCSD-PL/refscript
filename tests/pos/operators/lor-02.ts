
/*@ foo :: (x: undefined + number, y: { number | v > 0 }) => number */
export function foo(x,y) {
    if (y || x) {
        return 1;
    }
    else {
        return 0;
    }
}
