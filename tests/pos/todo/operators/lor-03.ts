


/*@ foo :: (x: undefined + { number | v > 1}, y: { number | v > 2}) => { number | v > 0 } */
export function foo(x, y) {
    let r = x || y;
    return r;
}
