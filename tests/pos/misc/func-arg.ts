// "Fatal error: exception Parsing.Parse_error"
/*@ foo :: (func:number) => number */
function foo(func:number) {
     return func
}

// SAFE
/*@ bar :: (xunc:number) => number */
function bar(xunc:number) {
     return xunc
}
