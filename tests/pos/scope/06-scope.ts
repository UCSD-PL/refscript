//adapted from navier-stokes

/*@ u :: string */
let u = "hi";

module A {

    export function bar(u: number): void {
        u = 3;
        u = 3;
    }

    /*@ blah :: () => {void | true} */
    function blah() {
        u = u + " there"
    }
}
