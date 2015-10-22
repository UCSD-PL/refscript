
/*@ foo :: (x:null, y:number, z: posint) => posint */
export function foo(x, y, z): number {
    if (x || y || z) {
        return 1;
    }
    else {
        return 0;
    }
}
