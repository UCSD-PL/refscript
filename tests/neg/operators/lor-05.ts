
/*@ foo :: (x: undefined + { number | v > 1}, y: { number | v > 2}) => { number | v > 2 } */
export function foo(x, y) {
    return x || y;
}
