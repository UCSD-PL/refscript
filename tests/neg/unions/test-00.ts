
/*@ fails :: (number) => number + undefined */
export function fails (x:number):any {
    return x ? true : undefined;
}
