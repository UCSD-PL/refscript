
// The cast from  MU --> UQ array should fail !


/*@ map :: <T, U>(array: IArray<T>, f: (x: T) => U) => MArray<U> + undefined */
export function map<T, U>(array: IArray<T>, f: (x: T) => U): MArray<U> {
    /*@ result :: MArray<U> + undefined */
    let result: MArray<U>;
    if (array) {
        let result_0: MArray<U> = [];
        let len = array.length;
        for (let i = 0; i < len; i++) {
            result_0.push(f(array[i]));
        }
        result = result_0;
    }


    return result;
}
