function foo(o, s) /*: { __proto__: {y: Bool}, x: Num, y: _ } * /(x|y)/ -> Num + Bool */ {
    return o[s];
}
