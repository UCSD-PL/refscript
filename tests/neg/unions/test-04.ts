
/*@ foo :: ({number | 0 < 1}) => { number | v = 2 } + boolean  */
function foo(x) {
    /*@ r :: number + boolean */
    let r: any = 1;
    if (x > 0) {
        r = 1;
    }
    else {
        r = true;
    }
    return r;
}
