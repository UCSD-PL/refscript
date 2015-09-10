


/*@ foo :: (x: undefined + number, y: { number | v > 0 }) => { number | true } */
function foo(x,y) {
    return y || x;
}

