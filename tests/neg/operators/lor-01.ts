
/*@ foo :: (x: undefined + number, y:number) => { number | v = 1 } */
export function foo(x,y) {
    if (x || y) {
        return 0;
    }
    else {
        return 1;
    }
}
