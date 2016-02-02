
/*@ readonly */
let innerObj = { n: 6 }

/*@ readonly */
let gobj = { a: 5, b: "String", oo: innerObj };

export function foo (): { n: number } {
    return gobj.oo;
}
