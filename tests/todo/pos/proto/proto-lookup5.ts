/*@ foo :: ({ __proto__: {y: boolean, __proto__: {}, *: _}, *: string, x: number }, 
            { string | (v = "x" || v = "y" || v = "z")}) => number + boolean + string + undefined */ 
function foo(o, s) {
    return o[s];
}
