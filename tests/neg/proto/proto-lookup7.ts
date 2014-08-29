
/*@ foo :: ({ __proto__: { x: string }, x: number }) => { string | true } */
function foo(o) {
    return o.x;
}

