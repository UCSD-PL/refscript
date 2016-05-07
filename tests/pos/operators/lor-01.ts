
/*@ foo :: (x: undefined + number, y:number, z: posint) => { number | v = 1 } */
export function foo(x, y, z) {
    if (x || y || z) {
        return 1;
    }
    else {
        return 0;
    }
}
