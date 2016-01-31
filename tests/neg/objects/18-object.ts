
/*@ gobj :: { a: posint } */
let gobj = { a: 5, b: "glorp" };

function foo() {
    gobj.a = gobj.a - 1;
}

export function moo(): void {
    foo();
    let z = gobj.a;
    assert(z > 0);
    return;
}
