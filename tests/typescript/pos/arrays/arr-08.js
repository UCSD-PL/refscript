/*@ arr :: [string] */ var arr = ["a"];

/*@ baz :: () => {v: string | true } */
function baz() {
    return arr[0];
}
