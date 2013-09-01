function foo(o) /*: 
    #{*: Str, /(a*)/: Bool, /b(b*)/: Bool}
->  #{*: Bool + Str, /(a*|b)/: Num + Bool} */ {
    return o;
}
