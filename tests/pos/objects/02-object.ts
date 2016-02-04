/*@ qualif PLusOne(v: int, w: int): v = w + 1 */

/*@ local innerObj :: { n: number } */
let innerObj = { n: 6 };

/*@ outerObj :: { a: number; b: string; c: { n : number } } */
let outerObj = { a: 5, b: "String", c: innerObj };

export function foo (n: number): { n: number } {
    return outerObj.c;
}
