// Taken from strobe

/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnumP (p: Str, x: a): enumProp x p */

/*@  foo :: (o: { [x:string]: string + number }) => { number | 0 < 1 } */
export function foo(o: { [x: string]: string | number }): any {
    for (let x in o) {
        let r = o[x];
        if (typeof r === "string") {
            return r;
        }
    }
    return "no string found";
};
