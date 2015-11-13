


/*@ foo :: (x: undefined + number, y: { number | v > 0 }) => { number | 0 < 1 } */
function foo(x,y) {
    return y || x;
}

