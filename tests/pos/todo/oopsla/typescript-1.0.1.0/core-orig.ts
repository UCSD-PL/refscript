/// <reference path="types.ts"/>

module ts {
    export interface Map<T> {
        [index: string]: T;
    }

    export interface StringSet extends Map<any> { }

    export function forEach<T, U>(array: T[], callback: (element: T) => U): U {
        var result: U;
        if (array) {
            for (var i = 0, len = array.length; i < len; i++) {
                if (result = callback(array[i])) break;
            }
        }
        return result;
    }

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

    export function filter<T>(array: T[], f: (x: T) => boolean): T[] {
        var result: T[];
        if (array) {
            result = [];
            for (var i = 0, len = array.length; i < len; i++) {
                var item = array[i];
                if (f(item)) {
                    result.push(item);
                }
            }
        }
        return result;
    }

    export function map<T, U>(array: T[], f: (x: T) => U): U[] {
        var result: U[];
        if (array) {
            result = [];
            var len = array.length;
            for (var i = 0; i < len; i++) {
                result.push(f(array[i]));
            }
        }
        return result;
    }

    export function concatenate<T>(array1: T[], array2: T[]): T[] {
        if (!array2 || !array2.length) return array1;
        if (!array1 || !array1.length) return array2;
        return array1.concat(array2);
    }

    export function sum(array: any[], prop: string): number {
        var result = 0;
        for (var i = 0; i < array.length; i++) {
            result += array[i][prop];
        }
        return result;
    }

    export function binarySearch(array: number[], value: number): number {
        var low = 0;
        var high = array.length - 1;

        while (low <= high) {
            var middle = low + ((high - low) >> 1);
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

        return ~low;
    }

    export declare function hasProperty<T>(map: Map<T>, key: string): boolean;

    export declare function getProperty<T>(map: Map<T>, key: string): T;

    export function isEmpty<T>(map: Map<T>) {
        for (var id in map) {
            if (hasProperty(map, id)) {
                return false;
            }
        }
        return true;
    }

    export function forEachValue<T, U>(map: Map<T>, callback: (value: T) => U): U {
        var result: U;
        for (var id in map) {
            if (result = callback(map[id])) break;
        }
        return result;
    }

    export function forEachKey<T, U>(map: Map<T>, callback: (key: string) => U): U {
        var result: U;
        for (var id in map) {
            if (result = callback(id)) break;
        }
        return result;
    }

    export function mapToArray<T>(map: Map<T>): T[] {
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
    export function arrayToMap<T>(array: T[], makeKey: (value: T) => string): Map<T> {
        var result: Map<T> = {};

        forEach(array, value => {
            result[makeKey(value)] = value;
        });

        return result;
    }

    export function compareValues<T>(a: T, b: T): number {
        if (a === b) return 0;
        if (a === undefined) return -1;
        if (b === undefined) return 1;
        return a < b ? -1 : 1;
    }

    export interface ObjectAllocator {
        getNodeConstructor(kind: SyntaxKind): new () => Node;
        getSymbolConstructor(): new (flags: SymbolFlags, name: string) => Symbol;
        getTypeConstructor(): new (checker: TypeChecker, flags: TypeFlags) => Type;
        getSignatureConstructor(): new (checker: TypeChecker) => Signature;
    }

    function Type(checker: TypeChecker, flags: TypeFlags) {  this.flags = flags;  }
}
