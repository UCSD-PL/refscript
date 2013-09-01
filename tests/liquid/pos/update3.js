function foo(o, s) /*: {/x/: Num, /y/: Str + Num} * /y/ -> Undef */ {
    o[s] = "a string";
}
