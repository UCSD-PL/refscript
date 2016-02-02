
/*@ readonly */
let x = {
    a: 5,
    b: "String",
};

/*@ foo :: () => { v: number | v = 4 } */
export function foo() {
    let y = { a: 4 }
    y = x;
    return y.a;
}
