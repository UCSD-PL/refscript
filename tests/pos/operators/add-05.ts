
/*@ myPlusOk :: (x: number, y: number) => { number | v = x + y }  */
/*@ myPlusOk :: (x: number + string, y: number + string) => string */
function myPlusOk(x, y) {
    return myPlusOk(x, y);
}


/*@ num_num :: (a:number, b:number) => {number | v = a + b } */
function num_num(a: number, b: number) {
    let d: number = myPlusOk(a, b);
    return d;
}

/*@ str_str :: (string) => {string | true } */
function str_str(a: string): string {
    let b: string = "dog";
    return myPlusOk(a, b);
}

/*@ num_str :: (number) => {string | true } */
export function num_str(a: number): string {
    let b: string = "dog";
    return myPlusOk(a, b);
}



