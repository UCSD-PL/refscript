function foo(o, s) 
/*:   { __proto__: {y: Bool, __proto__: Object, *: _}, *: Str, x: Num } 
    * /(x|(y|z))/ 
   -> Num + Bool + Str + Undef */ {
    return o[s];
}
