// Taken from strobe

/*@ qualif HasP<A>(x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP<A>(x: string, y: A): enumProp(x, y) */

/*@ foo :: (o: { [x:string]: string + number }) => { string | 0 < 1 } */
function foo(o) {
    for (var x in o) {
        var r = o[x];
        if (typeof r === "string") {
            return r;
        }
    }
    return "no string found";
};
