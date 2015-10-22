
/*@ myPlusOk :: (x: number, y: number) => { number | v = x + y }  */
/*@ myPlusOk :: (x: number + string, y: number + string) => string */
export function myPlusOk(x, y) {
    return myPlusOk(x, y);
}

/*@ assertEqual :: (x:number, y:number) => void */
function assertEqual(x: number, y: number) {
    assert(x === y);
}

/*@ num_one :: (a:number) => {void | true} */
function num_one(a: number): void {
    let d = myPlusOk(0, 1);
    assertEqual(d, 1);
    return;
}
