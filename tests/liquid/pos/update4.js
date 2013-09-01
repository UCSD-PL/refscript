// Assigning to star
function foo(o, s) /*: { *: Bool, /x/: Num, /y/: Str + Num} * /(z*)/ -> Undef */ {
    o[s] = true;
}
