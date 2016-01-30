
/*@ qualif Length(v: a): len v = 1  */


/*@ readonly arr :: IArray<string> */
let arr = ["a"];

export function baz(): string {
    return arr[0];
}

baz();
