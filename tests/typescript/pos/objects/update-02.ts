/*@ foo ::  ({ x: number, *: string }) => { void | true } */ 
function foo(o):void { 
    o.y = "aaa";
}
