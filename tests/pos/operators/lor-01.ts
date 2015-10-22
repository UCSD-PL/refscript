
/*@ foo :: (x: undefined + number, y:number) => { number | v = 0 } */
export function foo(x,y) {
    if (x || y) {
        return 0;
    }
    else {
        return 1;
    }
}
