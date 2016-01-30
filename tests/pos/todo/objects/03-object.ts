
/*@ readonly */
let gobj = { a: 5, b: "String", oo: { n: 6 } };

export function foo (): { n: number } {
    return gobj.oo;
}
