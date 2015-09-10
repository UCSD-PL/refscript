


/*@ foo :: (x: undefined + number, y:number) => { number | true } */
function foo(x,y) {
    return x || y;
}

