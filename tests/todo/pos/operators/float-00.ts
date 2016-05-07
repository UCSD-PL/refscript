
/*@ foo :: (x:number) => {number | 0 < 1} */
function foo(x:number):number { 
    return x + 0.5;
}