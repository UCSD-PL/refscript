
/*@ readonly */
let innerObj = { c: 3 };

/*@ a06 :: { b: { c: number } } */
let a06 = { b: innerObj };

function foo(o: { c: number }): void {
    o.c = 2;
}

foo(a06.b);

assert(a06.b.c > 0);
