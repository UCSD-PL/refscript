/// <reference path="libs/types.ts"/>

module ts {
    // export interface StringSet extends Map<any> { }

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

    /*@ map :: <T, U>(array: IArray<T>, f: (x: T) => U) => UArray<U> + undefined */
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

    // /*@ concatenate :: forall T M . ( array1: IArray<T> + undefined, array2: IArray<T> + undefined) => { IArray<T> + undefined | 0 < 1 } */
    // export function concatenate<T>(array1: T[], array2: T[]): T[] {
    //     if (!array2 || !array2.length) return array1;
    //     if (!array1 || !array1.length) return array2;
    //     /*@ arr1 :: IArray<T> */
    //     let arr1 = array1;
    //     /*@ arr2 :: IArray<T> */
    //     let arr2 = array2;
    //     return arr1.concat(arr2);
    // }
    //
    // /*@ sum :: (array: IArray<{{[s:string]:number} | hasProperty(prop, v)}>, prop: string) => { number | 0 < 1 } */
    // export function sum(array: any[], prop: string): number {
    //     let result = 0;
    //     for (let i = 0; i < array.length; i++) {
    //         result += <number>(array[i][prop]);
    //     }
    //     return result;
    // }
    //
    // /*@ binarySearch :: (array: { IArray<number> | (len v) > 1 }, value: number) => { number | ((0 <= v && v < (len array)) || v = -1) } */
    // export function binarySearch(array: number[], value: number): number {
    //     let low = 0;
    //     let high = array.length - 1;
    //     while (low <= high) {
    //         let middle = low + ((high - low) >> 1);
    //         assume(low <= middle && middle <= high);
    //         let midValue = array[middle];
    //         if (midValue === value) {
    //             return middle;
    //         }
    //         else if (midValue > value) {
    //             high = middle - 1;
    //         }
    //         else {
    //             low = middle + 1;
    //         }
    //     }
    //     return -1; // ~low;     // PV: Not sure what the purpose of ~low is.
    // }
    //
    // /*@ hasProperty :: forall T M . (map: Map<M,T>, key: string) => { boolean | Prop(v) <=> (hasDirectProperty(key,map) && hasProperty(key,map)) } */
    // export declare function hasProperty<T>(map: Map<T>, key: string): boolean;
    //
    // /*@ getProperty :: forall T . (map: Map<Immutable,T>, key: string) => { T | 0 < 1 } */
    // export declare function getProperty<T>(map: Map<T>, key: string): T;
    //
    // /*@  qualif HSqualif<A>(s: string, v: A): hasProperty(s,v) */
    // /*@  qualif EPqualif<A>(s: string, v: A): enumProp(s,v) */
    //
    // /*@ isEmpty :: forall T . (map: Map<Immutable,T>) => { boolean | 0 < 1 } */
    // export function isEmpty<T>(map: Map<T>) {
    //     for (let id in map) {
    //         if (hasProperty(map, id)) {
    //             return false;
    //         }
    //     }
    //     return true;
    // }
    //
    //
    // /*@ forEachValue :: forall T U . (map: Map<Immutable,T>, callback: (value: T) => U) => { U | 0 < 1 } + undefined */
    // export function forEachValue<T, U>(map: Map<T>, callback: (value: T) => U): U {
    //     /*@ result :: U + undefined */
    //     let result: U;
    //     for (let id in map) {
    //         result = callback(map[id]);
    //     }
    //     return result;
    // }
    //
    //
    // /*@ forEachKey :: forall T U . (map: Map<Immutable,T>, callback: (key: string) => U) => { U | 0 < 1 } + undefined */
    // export function forEachKey<T, U>(map: Map<T>, callback: (key: string) => U): U {
    //     /*@ result :: U + undefined */
    //     let result: U;
    //     for (let id in map) {
    //         result = callback(id);
    //     }
    //     return result;
    // }
    //
    // /*@ mapToArray :: forall T . (map: Map<Immutable,T>) => { MArray<T> | 0 < 1 } */
    // export function mapToArray<T>(map: Map<T>): T[] {
    //     /*@ result :: MArray<T> */
    //     let result: T[] = [];
    //
    //     for (let id in map) {
    //         result.push(map[id]);
    //     }
    //
    //     return result;
    // }
    //
    // /**
    //  * Creates a map from the elements of an array.
    //  *
    //  * @param array the array of input elements.
    //  * @param makeKey a function that produces a key for a given element.
    //  *
    //  * This function makes no effort to avoid collisions; if any two elements produce
    //  * the same key with the given 'makeKey' function, then the element with the higher
    //  * index in the array will be the one associated with the produced key.
    //  */
    //
    // /*@ _toMap :: forall S T M . (obj: S) => Map<M,T> */
    // declare function _toMap(obj: any): Map<any>;
    //
    // /*@ arrayToMap :: forall T . (array: IArray<T>, makeKey: (value: T) => string) => { Map<Mutable,T> | 0 < 1 } */
    // export function arrayToMap<T>(array: T[], makeKey: (value: T) => string): Map<T> {
    //     /*@ readonly makeKeyLoc :: # */
    //     let makeKeyLoc = makeKey;
    //     /*@ readonly result :: Map<Mutable,T> */
    //     let result: Map<T> = _toMap({});
    //     forEach(array, function(value:T) /*@ <anonymous> (value: T) => void */ {
    //         result[makeKeyLoc(value)] = value;
    //     });
    //     return result;
    // }
    //
    // /*@ compareValues :: forall T . (a:T, b:T) => { number | 0 < 1 } */
    // export function compareValues<T>(a: T, b: T): number {
    //     if (a === b) return 0;
    //     if (a === undefined) return -1;
    //     if (b === undefined) return 1;
    //     return a < b ? -1 : 1;
    // }
    //
    // export interface ObjectAllocator {
    //     getNodeConstructor(kind: SyntaxKind): { new (): Node };
    //     getSymbolConstructor(): {
    //       /*@ new (flags: SymbolFlags, name: string) => { v: ISymbol | keyVal(v, "flags") = flags } */
    //       new (flags: SymbolFlags, name: string): Symbol
    //     };
    //     getTypeConstructor(): { new (checker: TypeChecker, flags: TypeFlags): Type };
    //     getSignatureConstructor(): { new (checker: TypeChecker): Signature };
    // }
    //
    // /*@ newType :: forall M . (checker: TypeChecker<Immutable>, flags: bitvector32) => { Type<M> | type_flags(flags,v) } */
    // export declare function newType(checker: TypeChecker, flags: TypeFlags): Type;
}
