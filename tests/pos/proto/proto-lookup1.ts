
/*@ foo :: ({ __proto__: { x: string }, *: number }) => number */
function foo(o) {
    return o.x;
}

