
/*@ foo :: (x: number) => { number | v > 100 } */
/*@ foo :: (x: string) => { number | v > 100 } */
function foo(x: any): any {
    if (typeof x === "number") {
        if (x > 100) {
            return x;
        }
    }
    return 200;
}
