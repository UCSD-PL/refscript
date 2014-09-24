/*@ foo1 :: (x:number) => { () => undefined | true } */
function foo1(x:number) {
    return function() { return undefined }
}
