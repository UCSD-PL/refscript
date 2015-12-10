function poo(x: number) {
    return 0;
}

/*@ myPlusOk :: (x:number, y:number) => {number | v = x + y} */
/*@ myPlusOk :: (x:number, y:string) => string */
/*@ myPlusOk :: (x:string, y:number) => string */
/*@ myPlusOk :: (x:string, y:string) => string */
function myPlusOk(x, y) {
    return myPlusOk(x, y);
}

/*@ one :: () => {number | v = 1} */
function one() {
    let d = myPlusOk(0, 1);
    assert(d === 1);
    return d;
}

/*@ num_one :: (a:number) => {number | v = a + 1} */
function num_one(a) {
    let d = myPlusOk(a, 1);
    return d;
}

export function num_str(a: number): string {
    let d = myPlusOk(0, "cat");
    return d;
}
