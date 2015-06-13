
/**
 *  ARRAY 
 *  
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L966
 *
 *    TODO: Fix mutabilities
 *    consult: https://github.com/UCSD-PL/RefScript/blob/develop/include/prelude.ts
 *
 */
 
/*@ measure len :: forall M A . (Array<M,A>) => number */


/*@ interface Array<M,T> */
interface Array<T> {

    toString(): string;

    toLocaleString(): string;

    /*@ concat: 
        /\ forall M0 . (this: IArray<T>, items: IArray<T>): { Array<M0,T> | (len v) = (len this) + (len items) }
        /\ forall M1 M2 . (items: Array<M1,T>): {Array<M2,T> | true}
    */
    concat<U extends T[]>(...items: U[]): T[];

    // concat(...items: T[]): T[];
  
    join(separator?: string): string;

    /*@ pop: (this: MArray<T>): {T | true} */
    pop(): T;

    /*@ push: (this: MArray<T>, items: T): {number | true} */
    push(T): number;								// push(...items: T[]): number;

    /*@ reverse: (): {Array<M,T> | true} */
    reverse(): T[];

    shift(): T;

    /*@ slice : /\ forall N . (start: number, end: number): Array<N,T>
                /\ forall N . (start: number): Array<N,T>
                /\ forall N . (): Array<N,T>
     */
    slice(start?: number, end?: number): T[];

    /*@  sort : 
        /\ ( ): { v : Array<M,T> | len(v) = len(this) } 
        /\ (compareFn: (a: T, b: T) => number): { v : Array<M,T> | len(v) = len(this) } 
     */
    sort(compareFn?: (a: T, b: T) => number): T[];

    splice(start: number): T[];

    // splice(start: number, deleteCount: number, ...items: T[]): T[];

    // unshift(...items: T[]): number;

    indexOf(searchElement: T, fromIndex?: number): number;

    lastIndexOf(searchElement: T, fromIndex?: number): number;

    every(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;

    some(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;

    forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;

    /*@ map : forall U. (callbackfn: (value: T) => U): {IArray<U> | true} */
    map<U>(callbackfn: (value: T) => U): U[];
    
    /*@ map : forall U. (callbackfn:(value: T, index: number) => U): {IArray<U> | true} */
    map<U>(callbackfn: (value: T, index: number) => U): U[];
    
    /*@ map : forall U. (callbackfn:(value: T, index: number, array: IArray<T>) => U): {IArray<U> | true} */
    map<U>(callbackfn: (value: T, index: number, array: T[]) => U): U[];

    /*@ filter : 
        /\ forall N . (callbackfn: (value: T) => boolean): {Array<N, T> | true}
        /\ forall N . (callbackfn: (value: T, index: number) => boolean): {Array<N, T> | true}
        /\ forall N . (callbackfn: (value: T, index: number, array: IArray<T>) => boolean): {Array<N, T> | true} */
    filter(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): T[];


    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;

    //TODO why does callbackfn have 4 args in the typescript annotation but only 3 in the refscript?
 
    /*@ reduce : forall U . (this: IArray<T>, callback: (x: U, y: T, n: {number | 0 <= v && v < len this}) => U, init: U): U */
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    length: number;

    /*@ _get_length_ : 
        /\ (this: Array<Immutable,T>): { v: number | v >= 0 && v = (len this) } 
        /\ (this: Array<M,T>): { v: number | v >= 0 } 
     */
    _get_length_(): number;

//      // [n: number]: T;
}

declare var Array: {

    /*@ forall M T . () => { v: Array<M, T> | (len v) = 0 } */
    (): any[];

    /*@ forall M T. (arrayLength: number) => { v: Array<M, T> | (len v) = arrayLength } */
    <T>(arrayLength: number): T[];

    /*@ new forall M T . () => { v: Array<M, T> | (len v) = 0 } */
    new (): any[];

    /*@ new forall M T. (arrayLength: number) => { v: Array<M, T> | (len v) = arrayLength } */
    new <T>(arrayLength: number): T[];

    // new <T>(...items: T[]): T[];

    
    // <T>(...items: T[]): T[];

    /*@ isArray: 
        /\ forall M T. (arg: Array<M,T>): { v: boolean | Prop(v) }
        /\ forall A . (arg: A): {boolean | true}
    */
    isArray(arg: any): boolean;

    prototype: Array<any>;
}



interface IArguments {
    [index: number]: any;
    length: number;
    // callee: Function;
}

