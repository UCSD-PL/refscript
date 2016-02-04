
/*@ innerObj :: (Mutable) { c: posint } */
let innerObj = { c: 3 };

/*@ outerObj :: (Mutable) { b: { c: number } } */
let outerObj = { b: innerObj };

function foo(o: { c: number }): void {
    o.c = 0;
}

foo(outerObj.b);

assert(outerObj.b.c > 0);
