
/*@ check_undefined :: <T>(T + undefined) => T */
function check_undefined<T>(x:any) : T {
    if (typeof x === "undefined")
        return crash();
    return <T>x;
}

check_undefined(1);
