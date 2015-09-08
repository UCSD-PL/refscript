
interface Array<M extends ReadOnly, T> {
    /**
      * Gets or sets the length of the array. This is a number one higher than the highest element defined in an array.
      */
    length: number;
    /**
      * Returns a string representation of an array.
      */
    toString(): string;
    toLocaleString(): string;
    /**
      * Appends new elements to an array, and returns the new length of the array.
      * @param items New elements of the Array.
      */
    // push<N>(...items: Array<T>): number;
    // /**
    //   * Removes the last element from an array and returns it.
    //   */
    // pop(): T;
    // /**
    //   * Combines two or more arrays.
    //   * @param items Additional items to add to the end of array1.
    //   */
    // concat<U extends T[]>(...items: U[]): T[];
    // /**
    //   * Combines two or more arrays.
    //   * @param items Additional items to add to the end of array1.
    //   */
    // concat(...items: T[]): T[];
    // /**
    //   * Adds all the elements of an array separated by the specified separator string.
    //   * @param separator A string used to separate one element of an array from the next in the resulting String. If omitted, the array elements are separated with a comma.
    //   */
    // join(separator?: string): string;
    // /**
    //   * Reverses the elements in an Array.
    //   */
    // reverse(): T[];
    // /**
    //   * Removes the first element from an array and returns it.
    //   */
    // shift(): T;
    // /**
    //   * Returns a section of an array.
    //   * @param start The beginning of the specified portion of the array.
    //   * @param end The end of the specified portion of the array.
    //   */
    // slice(start?: number, end?: number): T[];
    //
    // /**
    //   * Sorts an array.
    //   * @param compareFn The name of the function used to determine the order of the elements. If omitted, the elements are sorted in ascending, ASCII character order.
    //   */
    // sort(compareFn?: (a: T, b: T) => number): T[];
    //
    // /**
    //   * Removes elements from an array and, if necessary, inserts new elements in their place, returning the deleted elements.
    //   * @param start The zero-based location in the array from which to start removing elements.
    //   */
    // splice(start: number): T[];
    //
    // /**
    //   * Removes elements from an array and, if necessary, inserts new elements in their place, returning the deleted elements.
    //   * @param start The zero-based location in the array from which to start removing elements.
    //   * @param deleteCount The number of elements to remove.
    //   * @param items Elements to insert into the array in place of the deleted elements.
    //   */
    // splice(start: number, deleteCount: number, ...items: T[]): T[];
    //
    // /**
    //   * Inserts new elements at the start of an array.
    //   * @param items  Elements to insert at the start of the Array.
    //   */
    // unshift(...items: T[]): number;
    //
    // /**
    //   * Returns the index of the first occurrence of a value in an array.
    //   * @param searchElement The value to locate in the array.
    //   * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at index 0.
    //   */
    // indexOf(searchElement: T, fromIndex?: number): number;
    //
    // /**
    //   * Returns the index of the last occurrence of a specified value in an array.
    //   * @param searchElement The value to locate in the array.
    //   * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at the last index in the array.
    //   */
    // lastIndexOf(searchElement: T, fromIndex?: number): number;
    //
    // /**
    //   * Determines whether all the members of an array satisfy the specified test.
    //   * @param callbackfn A function that accepts up to three arguments. The every method calls the callbackfn function for each element in array1 until the callbackfn returns false, or until the end of the array.
    //   * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //   */
    // every(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    //
    // /**
    //   * Determines whether the specified callback function returns true for any element of an array.
    //   * @param callbackfn A function that accepts up to three arguments. The some method calls the callbackfn function for each element in array1 until the callbackfn returns true, or until the end of the array.
    //   * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //   */
    // some(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    //
    // /**
    //   * Performs the specified action for each element in an array.
    //   * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
    //   * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //   */
    // forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;
    //
    // /**
    //   * Calls a defined callback function on each element of an array, and returns an array that contains the results.
    //   * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
    //   * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //   */
    // map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];
    //
    // /**
    //   * Returns the elements of an array that meet the condition specified in a callback function.
    //   * @param callbackfn A function that accepts up to three arguments. The filter method calls the callbackfn function one time for each element in the array.
    //   * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //   */
    // filter(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): T[];
    //
    // /**
    //   * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //   * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
    //   * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //   */
    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // /**
    //   * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //   * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
    //   * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //   */
    // reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;
    //
    // /**
    //   * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //   * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
    //   * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //   */
    // reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // /**
    //   * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //   * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
    //   * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //   */
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    [n: number]: T;
}

type IArray<T> = Array<Immutable, T>

// interface ArrayConstructor {
//     new (arrayLength?: number): any[];
//     new <T>(arrayLength: number): T[];
//     new <T>(...items: T[]): T[];
//     (arrayLength?: number): any[];
//     <T>(arrayLength: number): T[];
//     <T>(...items: T[]): T[];
//     isArray(arg: any): arg is Array<any>;
//     prototype: Array<any>;
// }

// declare var Array: ArrayConstructor;


// interface IArray<T> extends Array<Immutable, T> {
//
//     /*@ concat:
//         /\ forall M0 . (this: IArray<T>, items: IArray<T>): { Array<M0,T> | (len v) = (len this) + (len items) }
//         /\ forall M1 M2 . (items: Array<M1, T>): { Array<M2, T> | true}
//      */
//     concat<U extends T[]>(...items: U[]): T[];
    //
    //     // concat(...items: T[]): T[];
    //
    //     join(separator?: string): string;
    //
    //     /*@ pop: (this: MArray<T>): {T | true} */
    //     pop(): T;
    //
    //     /*@ push: (this: MArray<T>, items: T): {number | true} */
    //     push(T): number;								// push(...items: T[]): number;
    //
    //     /*@ reverse: (): {Array<M,T> | true} */
    //     reverse(): T[];
    //
    //     shift(): T;
    //
    //     /*@ slice : /\ forall N . (start: number, end: number): Array<N,T>
    //                 /\ forall N . (start: number): Array<N,T>
    //                 /\ forall N . (): Array<N,T>
    //      */
    //     slice(start?: number, end?: number): T[];
    //
    //     /*@  sort :
    //         /\ ( ): { v : Array<M,T> | len(v) = len(this) }
    //         /\ (compareFn: (a: T, b: T) => number): { v : Array<M,T> | len(v) = len(this) }
    //      */
    //     sort(compareFn?: (a: T, b: T) => number): T[];
    //
    //     splice(start: number): T[];
    //
    //     // splice(start: number, deleteCount: number, ...items: T[]): T[];
    //
    //     // unshift(...items: T[]): number;
    //
    //     indexOf(searchElement: T, fromIndex?: number): number;
    //
    //     lastIndexOf(searchElement: T, fromIndex?: number): number;
    //
    //     every(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    //
    //     some(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    //
    //     forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;
    //
    //     /*@ map : forall U. (callbackfn: (value: T) => U): {IArray<U> | true} */
    //     map<U>(callbackfn: (value: T) => U): U[];
    //
    //     /*@ map : forall U. (callbackfn:(value: T, index: number) => U): {IArray<U> | true} */
    //     map<U>(callbackfn: (value: T, index: number) => U): U[];
    //
    //     /*@ map : forall U. (callbackfn:(value: T, index: number, array: IArray<T>) => U): {IArray<U> | true} */
    //     map<U>(callbackfn: (value: T, index: number, array: T[]) => U): U[];
    //
    //     /*@ filter :
    //         /\ forall N . (callbackfn: (value: T) => boolean): {Array<N, T> | true}
    //         /\ forall N . (callbackfn: (value: T, index: number) => boolean): {Array<N, T> | true}
    //         /\ forall N . (callbackfn: (value: T, index: number, array: IArray<T>) => boolean): {Array<N, T> | true} */
    //     filter(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): T[];
    //
    //
    //     // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    //
    //     //TODO why does callbackfn have 4 args in the typescript annotation but only 3 in the refscript?

//     /*@ reduce : forall U . (this: IArray<T>, callback: (x: U, y: T, n: {number | 0 <= v && v < len this}) => U, init: U): U */
//     reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;
//
//     /*@ _get_length_ :
//         /\ (this: Array<Immutable,T>): { v: number | v >= 0 && v = (len this) }
//         /\ (this: Array<M,T>): { v: number | v >= 0 }
//      */
//     _get_length_(): number;
//
// }

// declare var Array: {
//
//     /*@ forall M T . () => { v: Array<M, T> | (len v) = 0 } */
//     (): any[];
//
//     /*@ forall M T. (arrayLength: number) => { v: Array<M, T> | (len v) = arrayLength } */
//     <T>(arrayLength: number): T[];
//
//     /*@ new forall M T . () => { v: Array<M, T> | (len v) = 0 } */
//     new (): any[];
//
//     /*@ new forall M T. (arrayLength: number) => { v: Array<M, T> | (len v) = arrayLength } */
//     new <T>(arrayLength: number): T[];
//
//     // new <T>(...items: T[]): T[];
//
//
//     // <T>(...items: T[]): T[];
//
//     /*@ isArray:
//         /\ forall M T. (arg: Array<M,T>): { v: boolean | Prop(v) }
//         /\ forall A . (arg: A): {boolean | true}
//     */
//     isArray(arg: any): boolean;
//
//     prototype: Array<any>;
// }
//
//
//
// interface IArguments {
//     [index: number]: any;
//     length: number;
//     // callee: Function;
// }
