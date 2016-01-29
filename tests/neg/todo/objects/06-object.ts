
let a06 = { b: { c: 3 } };

function foo(o: { c: number }): void {
    o.c = 0;
}

foo(a06.b);

assert(a06.b.c > 0);
