
/*@ type iArray[T]    = #Array[#Immutable, T]        */

// TODO: *******MUST****** report error locations for malformed type-aliases!!!!

/*@ baz :: forall T. (#iArray) => {v:number | 0 < 1} */
function baz<T>(array: T[]): number {

    let i = 0;
    let n = array.length - 1;
    let p0 = array[0];
    let p1 = array[0];
    let pairs = new Array(n < 0 ? 0 : n);

    return i;
}
