
/*@ foo :: ({ __proto__: { x: string }, *: number }) => { string | true } */
function foo(o) {
    return o.x;
}

