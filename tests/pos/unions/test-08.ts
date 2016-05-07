
/*@ foo :: (x: number) => { number | v = 1 } + boolean  */
function foo(x) {
    /*@ global */ let r: number | boolean = 1;
    if (x > 0) {
        r = 1;
    }
    else {
        r = true;
    }
    return r;
}
