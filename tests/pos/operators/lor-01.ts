


/*@ foo :: (x: undefined + number, y:number) => { number | 0 < 1 } */
function foo(x,y) {
    return x || y;
}

