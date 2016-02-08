/*@ foo :: ( { __proto__: {y: boolean}, x: number}, 
      {string | ((v = "x") || (v = "y")) }) 
      => number + { boolean | 0 < 1 } */
function foo(o, s) {
    return o[s];
}
