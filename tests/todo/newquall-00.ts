
/* @local */ function foo(): number {
    return 10;
}
/*@ main :: (x: number ) => number */
function main(x:number):number {
    let a = foo();
    assert(a === 20);
    return a;
}
