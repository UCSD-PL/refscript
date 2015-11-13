/*@ foo :: (x: top) => { boolean | 0 < 1 } */
function foo(x:any): boolean {
    return (x === 3);
}

