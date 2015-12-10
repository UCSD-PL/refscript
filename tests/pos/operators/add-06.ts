
/*@ myPlusOk :: (x: number, y: number) => { number | v = x + y }  */
/*@ myPlusOk :: (x: number + string, y: number + string) => string */
export function myPlusOk(x, y) {
    return myPlusOk(x, y);
}

function assertEqual(x: number, y: number): void {
    assert(x === y);
}

export function num_one(a: number): void {
    let d = myPlusOk(0, 1);
    assertEqual(d, 1);
    return;
}
