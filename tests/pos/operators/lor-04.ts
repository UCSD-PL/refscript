
/*@ foo :: (x:null, y:number) => number */
export function foo(x,y): number {
    if (x || y) {
        return x;
    }
    else {
        return y;
    }
}
