
/*@ myPlusOk :: (x: number, y: number) => { number | v = x + y }  */
/*@ myPlusOk :: (x: number + string, y: number + string) => string */
export function myPlusOk(x, y) {
    return myPlusOk(x, y);
}

/*@ num_num :: (a: number, b: number) => { number | v = a + b } */
export function num_num(a, b) {
    let d = myPlusOk(a, b);
    return d;
}

export function str_str(a: string): string {
    let b = "dog";
    return myPlusOk(a, b);
}

export function num_str(a: number): string {
    let b = "dog";
    return myPlusOk(a, b);
}
