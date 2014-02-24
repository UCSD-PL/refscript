/*@ foo :: ( { __proto__: {y: boolean}, x: number}, 
      {string | ((v = "x") || (v = "y")) }) 
      => number + { boolean | true } */
function foo(o, s) {
    return o[s];
}
