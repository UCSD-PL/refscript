
// `innerObj` will be a unique reference so should not be
// cast to the type of `outerObj`

let innerObj = { n: 6 }

/*@ readonly */
let outerObj = { a: 5, b: "String", oo: innerObj };



module A {

    export function foo (): { n: number } {
        return outerObj.oo;
    }

}
