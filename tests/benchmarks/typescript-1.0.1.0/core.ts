/// <reference path="libs/types.ts"/>

/*@ qualif HSqualif(s: Str, v: a): hasProperty v s */
/*@ qualif EPqualif(s: Str, v: a): enumProp v s */

module cts {
    // export interface StringSet extends ts.Map<any> { }


    /*@ forEach :: <T, U>(array: IArray<T>, callback: (element: T) => U) => U + undefined */
    export function forEach<T, U>(array: IArray<T>, callback: (element: T) => U): U {
        /*@ result :: U + undefined */
        let result: U;
        if (array) {
            let cnt = false;
            let len = array.length;
            for (let i = 0; i < len && cnt; i++) {
                if (result = callback(array[i]))
                    cnt = false;
            }
        }
        return result;
    }

    export function contains<T>(array: IArray<T>, value: T): boolean {
        if (array) {
            let len = array.length;
            for (let i = 0; i < len; i++) {
                if (array[i] === value) {
                    return true;
                }
            }
        }
        return false;
    }

    export function indexOf<T>(array: IArray<T>, value: T): number {
        if (array) {
            let len = array.length;
            for (let i = 0; i < len; i++) {
                if (array[i] === value) {
                    return i;
                }
            }
        }
        return -1;
    }

    /*@ filter :: <T>(array: IArray<T>, f: (x: T) => boolean) => MArray<T> + undefined */
    export function filter<T>(array: IArray<T>, f: (x: T) => boolean): MArray<T> {
        /*@ result :: MArray<T> + undefined */
        let result: MArray<T>;
        if (array) {
            let result_1: MArray<T> = [];    //PV: modification
            let len = array.length;
            for (let i = 0; i < len; i++) {
                let item = array[i];
                if (f(item)) {
                    result_1.push(item);
                }
            }
            result = result_1;
        }
        return result;
    }

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

    /*@ concatenate :: <T>( array1: IArray<T> + undefined, array2: IArray<T> + undefined) => IArray<T> + undefined */
    export function concatenate<T>(array1: T[], array2: T[]): T[] {
        if (!array2 || !array2.length) return array1;
        if (!array1 || !array1.length) return array2;
        /*@ arr1 :: IArray<T> */
        let arr1 = array1;
        /*@ arr2 :: IArray<T> */
        let arr2 = array2;
        return arr1.concat(arr2);
    }

    /*@ sum :: (array: IArray<{ (Immutable) {[s:string]:number} | hasProperty v prop }>, prop: string) => number */
    export function sum(array: any[], prop: string): number {
        let result = 0;
        for (let i = 0; i < array.length; i++) {
            result += <number>(array[i][prop]);
        }
        return result;
    }

    /*@ binarySearch :: (array: { IArray<number> | (len v > 1) }, value: number) => { number | ((0 <= v && v < len array) || (v = -1)) } */
    export function binarySearch(array: number[], value: number): number {
        let low = 0;
        let high = array.length - 1;
        while (low <= high) {
            let middle = low + ((high - low) >> 1);
            assume(low <= middle && middle <= high);
            let midValue = array[middle];
            if (midValue === value) {
                return middle;
            }
            else if (midValue > value) {
                high = middle - 1;
            }
            else {
                low = middle + 1;
            }
        }
        return -1; // ~low;     // PV: Not sure what the purpose of ~low is.
    }

    /*@ hasProperty_ :: <M extends ReadOnly, T>(map: ts.Map<M,T>, key: string) => { boolean | Prop(v) <=> (hasDirectProperty map key && hasProperty map key) } */
    export declare function hasProperty_<M extends ReadOnly, TT>(map: ts.Map<M, TT>, key: string): boolean;

    /*@ getProperty :: <M extends ReadOnly, T>(map: ts.Map<Immutable,T>, key: string) => T */
    export declare function getProperty<M extends ReadOnly, T>(map: ts.Map<M, T>, key: string): T;

    /*@ isEmpty :: <T>(map: ts.Map<Immutable,T>) => boolean */
    export function isEmpty<M extends ReadOnly, T>(map: ts.Map<M, T>) {
        for (let id in map) {
            if (hasProperty_(map, id)) {
                return false;
            }
        }
        return true;
    }

    /*@ forEachValue :: <T, U>(map: ts.Map<Immutable,T>, callback: (value: T) => U) => U + undefined */
    export function forEachValue<M extends ReadOnly, T, U>(map: ts.Map<M, T>, callback: (value: T) => U): U {
        /*@ result :: U + undefined */
        let result: U;
        for (let id in map) {
            result = callback(map[id]);
        }
        return result;
    }

    /*@ forEachKey :: <T, U>(map: ts.Map<Immutable,T>, callback: (key: string) => U) => U + undefined */
    export function forEachKey<T, U>(map: ts.Map<Immutable, T>, callback: (key: string) => U): U {
        /*@ result :: U + undefined */
        let result: U;
        for (let id in map) {
            result = callback(id);
        }
        return result;
    }

    export function mapToArray<T>(map: ts.Map<Immutable, T>): MArray<T> {
        /*@ result :: MArray<T> */
        let result: T[] = [];

        for (let id in map) {
            result.push(map[id]);
        }

        return result;
    }

    /**
     * Creates a map from the elements of an array.
     *
     * @param array the array of input elements.
     * @param makeKey a function that produces a key for a given element.
     *
     * This function makes no effort to avoid collisions; if any two elements produce
     * the same key with the given 'makeKey' function, then the element with the higher
     * index in the array will be the one associated with the produced key.
     */

     /*@ _toMap :: <S, T>(obj: S) => ts.Map<Mutable, T> */
    declare function _toMap<S>(obj: S): ts.Map<Mutable, any>;

    export function arrayToMap<T>(array: IArray<T>, makeKey: (value: T) => string): ts.Map<Mutable, T> {
        /*@ readonly */
        let makeKeyLoc = makeKey;
        /*@ readonly result :: ts.Map<Mutable,T> */
        let result: ts.Map<Mutable, T> = _toMap({});

        let ff: (value: T) => void = function (value) {
            result[makeKeyLoc(value)] = value;
        }
        forEach(array, ff);
        return result;
    }

    export function compareValues<T>(a: T, b: T): number {
        if (a === b) return 0;
        if (a === undefined) return -1;
        if (b === undefined) return 1;
        return (a < b) ? (-1) : 1;
    }

    export interface ObjectAllocator<M extends ReadOnly> {
        getNodeConstructor(kind: ts.SyntaxKind): new () => ts.Node<M>;
        /*@ getSymbolConstructor(): { new (flags: ts.SymbolFlags, name: string): { v: ISymbol | keyVal(v, "flags") = flags } } */
        getSymbolConstructor()    : new (flags: ts.SymbolFlags, name: string) => ts.Symbol<M>;
        getTypeConstructor()      : new (checker: ts.TypeChecker<M>, flags: ts.TypeFlags) => ts.Type<M>;
        getSignatureConstructor() : new (checker: ts.TypeChecker<M>) => ts.Signature<M>;
    }

    /*@ newType :: <M>(checker: ts.TypeChecker<Immutable>, flags: bitvector32) => { ts.Type<M> | type_flags(flags,v) } */
    export declare function newType<M extends ReadOnly>(checker: ts.TypeChecker<Immutable>, flags: ts.TypeFlags): ts.Type<M>;
}
