/// <reference path="types.ts"/>

module cts {

    export interface StringSet<M extends ReadOnly> extends ts.Map<M, string> { }

    /*@ lookUp :: <T>(map: ts.Map<Immutable,T>, key: string) => { T | hasProperty map key } + undefined */
    export declare function lookUp<T>(map: ts.Map<Immutable, T>, key: string): T;
    //// TODO

    export declare function getLocaleSpecificMessage(message: string): string;
    //// TODO

    /*@ forEach :: <T, U>(array: IArray<T>, callback: (element: T) => U) => U + undefined */
    export declare function forEach<T, U>(array: IArray<T>, callback: (element: T) => U): U;

    export declare function contains<T>(array: IArray<T>, value: T): boolean;

    export declare function indexOf<T>(array: IArray<T>, value: T): number;

    /*@ filter :: <T>(array: IArray<T>, f: (x: T) => boolean) => MArray<T> + undefined */
    export declare function filter<T>(array: IArray<T>, f: (x: T) => boolean): MArray<T>;

    /*@ map :: <T, U>(array: IArray<T>, f: (x: T) => U) => MArray<U> + undefined */
    export declare function map<T, U>(array: IArray<T>, f: (x: T) => U): MArray<U>;

    /*@ concatenate :: <T>( array1: IArray<T> + undefined, array2: IArray<T> + undefined) => IArray<T> + undefined */
    export declare function concatenate<T>(array1: T[], array2: T[]): T[];

    /*@ sum :: (array: IArray<{ (Immutable) {[s:string]:number} | hasProperty v prop }>, prop: string) => number */
    export declare function sum(array: any[], prop: string): number;

    /*@ binarySearch :: (array: { IArray<number> | (len v > 1) }, value: number) => { number | ((0 <= v && v < len array) || (v = -1)) } */
    export declare function binarySearch(array: number[], value: number): number;

    /*@ hasProperty_ :: <M extends ReadOnly, T>(map: ts.Map<M,T>, key: string) => { boolean | Prop(v) <=> (hasDirectProperty map key && hasProperty map key) } */
    export declare function hasProperty_<M extends ReadOnly, TT>(map: ts.Map<M, TT>, key: string): boolean;

    /*@ getProperty :: <M extends ReadOnly, T>(map: ts.Map<Immutable,T>, key: string) => T */
    export declare function getProperty<M extends ReadOnly, T>(map: ts.Map<M, T>, key: string): T;

    /*@ isEmpty :: <T>(map: ts.Map<Immutable,T>) => boolean */
    export declare function isEmpty<M extends ReadOnly, T>(map: ts.Map<M, T>): boolean;

    /*@ forEachValue :: <T, U>(map: ts.Map<Immutable,T>, callback: (value: T) => U) => U + undefined */
    export declare function forEachValue<M extends ReadOnly, T, U>(map: ts.Map<M, T>, callback: (value: T) => U): U;

    /*@ forEachKey :: <T, U>(map: ts.Map<Immutable,T>, callback: (key: string) => U) => U + undefined */
    export declare function forEachKey<T, U>(map: ts.Map<Immutable, T>, callback: (key: string) => U): U;

    export declare function mapToArray<T>(map: ts.Map<Immutable, T>): MArray<T>;

     /*@ _toMap :: <S, T>(obj: S) => ts.Map<Mutable, T> */
    declare function _toMap<S>(obj: S): ts.Map<Mutable, any>;

    export declare function arrayToMap<T>(array: IArray<T>, makeKey: (value: T) => string): ts.Map<Mutable, T>;

    export declare function compareValues<T>(a: T, b: T): number;

    export interface ObjectAllocator<M extends ReadOnly> {
        getNodeConstructor(kind: ts.SyntaxKind): new () => ts.Node<M>;
        /*@ getSymbolConstructor(): { new (flags: ts.SymbolFlags, name: string): { v: ISymbol | offset v "flags" = flags } } */
        getSymbolConstructor()    : new (flags: ts.SymbolFlags, name: string) => ts.Symbol<M>;
        getTypeConstructor()      : new (checker: ts.TypeChecker<M>, flags: ts.TypeFlags) => ts.Type<M>;
        getSignatureConstructor() : new (checker: ts.TypeChecker<M>) => ts.Signature<M>;
    }

    /*@ newType :: <M>(checker: ts.TypeChecker<Immutable>, flags: bitvector32) => { ts.Type<M> | type_flags(flags,v) } */
    export declare function newType<M extends ReadOnly>(checker: ts.TypeChecker<Immutable>, flags: ts.TypeFlags): ts.Type<M>;
}
