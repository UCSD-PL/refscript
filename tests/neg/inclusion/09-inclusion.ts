/*@ qualif HasP(v: string, s: a): hasProperty(v,s) */
/*@ qualif EnumP(v: string, s: a): enumProp(v,s)    */

/*@ foo :: (o: (Immutable) { [x:string]: string }) => MArray<string> */
export function foo(o) {

    /*@ readonly ret :: MArray<string> */
    let ret = [];

    for (let x in o) {
        ret.push(o[x]);
    }
    ret.push(o["o"]);

    return ret;

};
