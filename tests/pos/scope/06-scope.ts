//adapted from navier-stokes

export function foo(): void {
    /*@ u :: string */
    let u = "hi";

    /*@ bar :: (u:number) => {void | true} */
    function bar(u) {
        u = 3;
        u = 3;
    }

    /*@ blah :: () => {void | true} */
    function blah() {
        u = u + " there"
    }
}
