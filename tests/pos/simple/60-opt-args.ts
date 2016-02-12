
function foo(l?: number)  {
    let lo = l;

    if (arguments.length < 1) {
        lo = 0;
    }
    let lo1 = <number>lo;
    assert(0 <= lo1);
}


foo();
foo(0);
foo(1);
