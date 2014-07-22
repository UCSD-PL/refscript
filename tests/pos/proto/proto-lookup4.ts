function foo(o, s) 
/*:   { __proto__: {y: Bool, __proto__: Object, z: _ }, y: _, z: _, x: Num} 
    * /(x|(y|z))/ 
   -> Num + Bool + Undef */ {
    return o[s];
}
