function foo(o) /*: #{x : Num, y : _, __proto__ : { y: _, __proto__: { y: _, __proto__: {y : Bool} } } } -> #{y : ^Bool} */ {
    return o;
}
