
interface Array<M extends ReadOnly, T> {
    length: number;

    /*@ @Immutable __getLength(): { number | v = len this } */
    /*@            __getLength(): number */
    __getLength(): number;

    /*@ @Mutable push(x: T): number */
    push(x: T): number;
    // push<N>(...items: Array<T>): number;

    /*@ @Mutable pop(): T */
    pop(): T;

    /*@ @Immutable concat     (item: IArray<T> ): { IArray<T> | len v = len this + len item } */
    /*@            concat<M,N>(item: Array<M,T>):   Array<N,T> */
    concat(item: T[]): T[];
    // concat<U extends T[]>(...items: U[]): T[];
    // concat(...items: T[]): T[];

    /*@ @Mutable reverse(): Array<M, T> */
    reverse(): T[];

    // toString(): string;
    // toLocaleString(): string;
    // join(separator?: string): string;
    // shift(): T;
    // slice(start?: number, end?: number): T[];
    // sort(compareFn?: (a: T, b: T) => number): T[];
    // splice(start: number): T[];
    // splice(start: number, deleteCount: number, ...items: T[]): T[];
    // unshift(...items: T[]): number;
    // indexOf(searchElement: T, fromIndex?: number): number;
    // lastIndexOf(searchElement: T, fromIndex?: number): number;
    // every(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    // some(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): boolean;
    // forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;

    /*@ map<U>(callbackfn: (value: T) => U): UArray<U> */
    /*@ map<U>(callbackfn: (value: T, index: number) => U): UArray<U> */
    map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];

    /*@ filter (callbackfn: (v: T) => boolean): UArray<T> */
    /*@ filter (callbackfn: (v: T, i: number) => boolean): UArray<T> */
    /*@ filter (callbackfn: (v: T, i: number, a: IArray<T>) => boolean): UArray<T> */
    filter(callbackfn: (value: T, index: number, array: T[]) => boolean, thisArg?: any): T[];

    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;
    // reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    [n: number]: T;
}

declare type UArray<T>  = Array<Unique, T>;
declare type IArray<T>  = Array<Immutable, T>;
declare type MArray<T>  = Array<Mutable, T>;
declare type ROArray<T> = Array<ReadOnly, T>;


// XXX: Add Well formedness check for missing type params

/*@ builtin_getLength ::                     <T>(a: IArray<T>) =>  { v: number | v >= 0 && v = (len a) } */
/*@ builtin_getLength :: <M extends ReadOnly, T>(a: Array<M,T>) => { v: number | v >= 0 } */
declare function builtin_getLength<M extends ReadOnly,T>(a: Array<M, T>): number;

interface ArrayConstructor<M extends ReadOnly> {
    // new (arrayLength?: number): any[];

    // XXX: Keep the array length refinement ???

    /*@ new <T>(arrayLength: number): { v: Array<Unique,T> | len v = arrayLength } */
    new <T>(arrayLength: number): Array<M,T>;

    // new <T>(...items: T[]): T[];
    // (arrayLength?: number): any[];
    // <T>(arrayLength: number): T[];
    // <T>(...items: T[]): T[];
    // isArray(arg: any): arg is Array<any>;
    // prototype: Array<any>;
}

declare let Array: ArrayConstructor<ReadOnly>;
