
/*@ foo :: (x: number, y: (number) => number) => {number + undefined | 0 < 1} */
/*@ foo :: (x: number) => {number + undefined | 0 < 1} */
function foo(x: any, y?: any): any {
    let bobZooo = arguments.length;
    assert (bobZooo > 0);
    return 1;
}
