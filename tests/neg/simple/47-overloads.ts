
/*@ foo :: (x: number) => { number | v > 100 } */
/*@ foo :: (x: string) => { number | v > 100 } */
export function foo(x: any): any {
    if (typeof x === "number") {
        if (x > 99) {
            return x;
        }
    }
    return 200;
}
