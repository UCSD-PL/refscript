function foo(o, s) /*: {/x/: Num, /y/: Str + Num} * /(x|y)/ -> Undef */ {
    o[s] = 10;
}
