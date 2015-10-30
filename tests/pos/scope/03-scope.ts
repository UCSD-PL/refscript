
export function foo(): void {
    /*@ gggg :: { number | v > 0  } */
    let gggg = 1;
    gggg = gggg + 1;
}

/*@ bar :: () => { number | v = 2 } */
function bar() {
    let s = 1;
    /*@ gggg :: { string | v = "a" } */
    let gggg = "a";
    s = s + 1;
    gggg = "a";
    return s;
}
