
/*@ foo :: ({ __proto__: { x: string }, y: number }) => string */
function foo(o) {
    return o.x;
}

