
/*@ readonly */
let innerObj = { n: 6 }

/*@ readonly */
let gobj = { a: 5, b: "String", oo: innerObj };



module A {

    /*@ foo :: () => { n: posint } */
    export function foo (): { n: number } {
        return gobj.oo;
    }

}
