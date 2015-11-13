
/*@ foo :: ({ __proto__: { x: string }, x: number }) => { string | 0 < 1 } */
function foo(o) {
    return o.x;
}

