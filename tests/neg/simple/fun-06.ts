/*@ foo1 :: (x:number) =>{  () => { undefined | 0 < 1 } } */
function foo1(x:number) {
    return function() { return undefined }
}
