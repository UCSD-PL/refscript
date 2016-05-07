
/*@ foo :: (x:number) => IArray<{ number | v = x }>  */
function foo(x: number): number[] {
    return [x];
}

/*@ bar :: (y:number) => IArray<{ number | v = y }>  */
export function bar(y: number): number[] {
    return foo(y);
}
