
/*@ innerObj :: (Mutable) { c: posint } */
let innerObj = { c: 3 };

/*@ outerObj :: (Mutable) { b: (Mutable) { c: number } } */
let outerObj = { b: innerObj };

/*@ foo :: (o: (Mutable) { c: number }) => void */
function foo(o: { c: number }): void {
    o.c = 0;
}

foo(outerObj.b);

assert(outerObj.b.c > 0);
