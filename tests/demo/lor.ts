
/*@ foo :: (x: undefined + number, y: number) => { number | 0 < 1 } */
function foo(x,y) {
    // Try changing the operator to '&&' - it fails because now
    // if x is undefined, x (and not a number) gets returned
    var r = x || y;
    return r; 
}
