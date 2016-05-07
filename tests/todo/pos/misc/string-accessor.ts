// Something like this is used in raytrace-typed-octane.ts to allow the user
// to specify any subset of a large list of optional parameters:

/*@ extend :: (dest:[Mutable]{[s:string]:top}, src:[ReadOnly]{[s:string]:top}) => {[Mutable]{[s:string]:top} | 0 < 1} */
function extend(dest, src) {
    for (var p in src) {
        dest[p] = src[p];
    }
    return dest;
}

var defaults = {a:0, b:0};
extend(defaults, {b:5});

assert(defaults.a === 0);
assert(defaults.b === 5);
