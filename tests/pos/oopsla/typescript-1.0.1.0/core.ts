/// <reference path="types.ts"/>

module ts {
    export interface StringSet extends Map<any> { }

    /*@ forEach :: forall T U . (array: IArray<T>, callback: (element: T) => U) => { U | true } + undefined */
    export function forEach<T, U>(array: T[], callback: (element: T) => U): U {
        /*@ result :: U + undefined */
        var result: U;
        if (array) {
            var cnt = false;
            for (var i = 0, len = array.length; i < len && cnt; i++) {
                if (result = callback(array[i])) 
                    cnt = false;
            }
        }
        return result;
    }

    /*@  contains :: forall T . (array: IArray<T>, value: T) => { boolean | true } */
    export function contains<T>(array: T[], value: T): boolean {
        if (array) {
            var len = array.length;
            for (var i = 0; i < len; i++) {
                if (array[i] === value) {
                    return true;
                }
            }
        }
        return false;
    }

    /*@  indexOf :: forall T . (array: IArray<T>, value: T) => { number | true } */
    export function indexOf<T>(array: T[], value: T): number {
        if (array) {
            var len = array.length;
            for (var i = 0; i < len; i++) {
                if (array[i] === value) {
                    return i;
                }
            }
        }
        return -1;
    }

    /*@ filter :: forall T . (array: IArray<T>, f: (T) => boolean) => { MArray<T> | true } + undefined */
    export function filter<T>(array: T[], f: (x: T) => boolean): T[] {
        /*@ result :: MArray<T> + undefined */
        var result: T[];
        if (array) {
        var result_1 = [];    //PV: modification
            for (var i = 0, len = array.length; i < len; i++) {
                var item = array[i];
                if (f(item)) {
                    result_1.push(item);
                }
            }
            result = result_1;
        }
        return result;
    }

    /*@ map :: forall T U . (array: IArray<T>, f: (x:T)=>U) => {MArray<U> | true} + undefined */
    export function map<T, U>(array: T[], f: (x: T) => U): U[] {
      /*@ result :: MArray<U> + undefined */
        var result: U[];
        if (array) {
            var result_0 = [];
            var len = array.length;
            for (var i = 0; i < len; i++) {
                result_0.push(f(array[i]));
            }
            result = result_0;
        }
        return result;
    }

    /*@ concatenate :: forall T M . ( array1: IArray<T> + undefined, array2: IArray<T> + undefined) => { IArray<T> + undefined | true } */
    export function concatenate<T>(array1: T[], array2: T[]): T[] {
        if (!array2 || !array2.length) return array1;
        if (!array1 || !array1.length) return array2;
        /*@ arr1 :: IArray<T> */
        var arr1 = array1;
        /*@ arr2 :: IArray<T> */
        var arr2 = array2;
        return arr1.concat(arr2);
    }

    /*@ sum :: (array: IArray<{{[s:string]:number} | hasProperty(prop, v)}>, prop: string) => { number | true } */
    export function sum(array: any[], prop: string): number {
        var result = 0;
        for (var i = 0; i < array.length; i++) {
            result += <number>(array[i][prop]);
        }
        return result;
    }

    /*@ binarySearch :: (array: { IArray<number> | (len v) > 1 }, value: number) => { number | ((0 <= v && v < (len array)) || v = -1) } */
    export function binarySearch(array: number[], value: number): number {
        var low = 0;
        var high = array.length - 1;
        while (low <= high) {
            var middle = low + ((high - low) >> 1);
            assume(low <= middle && middle <= high);
            var midValue = array[middle];
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

    /*@ hasProperty :: forall T M . (map: Map<M,T>, key: string) => { boolean | Prop(v) <=> (hasDirectProperty(key,map) && hasProperty(key,map)) } */
    export declare function hasProperty<T>(map: Map<T>, key: string): boolean;

    /*@ getProperty :: forall T . (map: Map<Immutable,T>, key: string) => { T | true } */
    export declare function getProperty<T>(map: Map<T>, key: string): T;

    /*@  qualif HSqualif(s:Str,v:a): hasProperty(s,v) */
    /*@  qualif EPqualif(s:Str,v:a): enumProp(s,v) */

    /*@ isEmpty :: forall T . (map: Map<Immutable,T>) => { boolean | true } */
    export function isEmpty<T>(map: Map<T>) {
        for (var id in map) {
            if (hasProperty(map, id)) {
                return false;
            }
        }
        return true;
    }


    /*@ forEachValue :: forall T U . (map: Map<Immutable,T>, callback: (value: T) => U) => { U | true } + undefined */
    export function forEachValue<T, U>(map: Map<T>, callback: (value: T) => U): U {
        /*@ result :: U + undefined */ 
        var result: U;
        for (var id in map) {
            result = callback(map[id]);
        }
        return result;
    }


    /*@ forEachKey :: forall T U . (map: Map<Immutable,T>, callback: (key: string) => U) => { U | true } + undefined */
    export function forEachKey<T, U>(map: Map<T>, callback: (key: string) => U): U {
        /*@ result :: U + undefined */
        var result: U;
        for (var id in map) {
            result = callback(id);
        }
        return result;
    }

    /*@ mapToArray :: forall T . (map: Map<Immutable,T>) => { MArray<T> | true } */
    export function mapToArray<T>(map: Map<T>): T[] {
        /*@ result :: MArray<T> */
        var result: T[] = [];

        for (var id in map) {
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

    /*@ _toMap :: forall S T M . (obj: S) => Map<M,T> */
    declare function _toMap(obj: any): Map<any>;

    /*@ arrayToMap :: forall T . (array: IArray<T>, makeKey: (value: T) => string) => { Map<Mutable,T> | true } */
    export function arrayToMap<T>(array: T[], makeKey: (value: T) => string): Map<T> {
        /*@ readonly makeKeyLoc :: # */
        var makeKeyLoc = makeKey;
        /*@ readonly result :: Map<Mutable,T> */
        var result: Map<T> = _toMap({});
        forEach(array, function(value:T) /*@ <anonymous> (value: T) => void */ {
            result[makeKeyLoc(value)] = value;
        });
        return result;
    }

    /*@ compareValues :: forall T . (a:T, b:T) => { number | true } */
    export function compareValues<T>(a: T, b: T): number {
        if (a === b) return 0;
        if (a === undefined) return -1;
        if (b === undefined) return 1;
        return a < b ? -1 : 1;
    }

    export interface ObjectAllocator {
        getNodeConstructor(kind: SyntaxKind): { new (): Node };
        getSymbolConstructor(): { 
          /*@ new (flags: SymbolFlags, name: string) => { v: ISymbol | keyVal(v, "flags") = flags } */
          new (flags: SymbolFlags, name: string): Symbol 
        };
        getTypeConstructor(): { new (checker: TypeChecker, flags: TypeFlags): Type };
        getSignatureConstructor(): { new (checker: TypeChecker): Signature };
    }
      
    /*@ newType :: forall M . (checker: TypeChecker<Immutable>, flags: bitvector32) => { Type<M> | type_flags(flags,v) } */
    export declare function newType(checker: TypeChecker, flags: TypeFlags): Type;
}
