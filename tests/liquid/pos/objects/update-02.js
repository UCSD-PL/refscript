/*@ foo ::  ({ x: number, *: string }) => { void | true } */ 
function foo(o) { 
    o.y = "aaa";
}
