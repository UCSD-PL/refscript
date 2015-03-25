/// <reference path="types.ts"/>

module ts {

    export interface StringSet extends Map<any> { }

    /*@ forEach :: forall T U . (array: IArray<T>, callback: (element: T) => U) => { U | true } + undefined */
    export declare function forEach<T, U>(array: T[], callback: (element: T) => U): U;

    /*@  contains :: forall T . (array: IArray<T>, value: T) => { boolean | true } */
    export declare function contains<T>(array: T[], value: T): boolean;

    /*@  indexOf :: forall T . (array: IArray<T>, value: T) => { number | true } */
    export declare function indexOf<T>(array: T[], value: T): number;

    /*@  filter :: forall T . (array: IArray<T>, f: (T) => boolean) => { MArray<T> | true } + undefined */
    export declare function filter<T>(array: T[], f: (x: T) => boolean): T[];

    /*@ map :: forall T U . (array: IArray<T>, f: (x:T)=>U) => {MArray<U> | true} + undefined */
    export declare function map<T, U>(array: T[], f: (x: T) => U): U[];

    /*@ concatenate :: forall T M . ( array1: IArray<T> + undefined, array2: IArray<T> + undefined) => { IArray<T> + undefined | true } */
    export declare function concatenate<T>(array1: T[], array2: T[]): T[];

    /*@ sum :: (array: IArray<{{[s:string]:number} | hasProperty(prop, v)}>, prop: string) => { number | true } */
    export declare function sum(array: any[], prop: string): number;

    /*@ binarySearch :: (array: { IArray<number> | (len v) > 1 }, value: number) => { number | ((0 <= v && v < (len array)) || v = -1) } */
    export declare function binarySearch(array: number[], value: number): number;

    /*@ hasProperty :: forall T M . (map: Map<M,T>, key: string) => { boolean | Prop(v) <=> (hasDirectProperty(key,map) && hasProperty(key,map)) } */
    export declare function hasProperty<T>(map: Map<T>, key: string): boolean;
    //// TODO

    /*@ getProperty :: forall T . (map: Map<Immutable,T>, key: string) => { T | true } */
    export declare function getProperty<T>(map: Map<T>, key: string): T;
    //// TODO 

    /*  qualif HSqualif(s:Str,v:a): hasProperty(s,v) */
    /*  qualif EPqualif(s:Str,v:a): enumProp(s,v) */

    /*@ isEmpty :: forall T . (map: Map<Immutable,T>) => { boolean | true } */
    export declare function isEmpty<T>(map: Map<T>): boolean;

    /*@ clone :: forall T . (T) => { T | true } */
    export declare function clone<T>(object: T): T;
    //// TODO -- barely typed

    /*@ forEachValue :: forall T U . (map: Map<Immutable,T>, callback: (value: T) => U) => { U | true } + undefined */
    export declare function forEachValue<T, U>(map: Map<T>, callback: (value: T) => U): U;

    /*@ forEachKey :: forall T U . (map: Map<Immutable,T>, callback: (key: string) => U) => { U | true } + undefined */
    export declare function forEachKey<T, U>(map: Map<T>, callback: (key: string) => U): U;

    /*@ lookUp :: forall T . (map: Map<Immutable,T>, key: string) => { T | hasProperty(key,map) } + undefined */ 
    export declare function lookUp<T>(map: Map<T>, key: string): T;
    //// TODO

    /*@ mapToArray :: forall T . (map: Map<Immutable,T>) => { MArray<T> | true } */
    export declare function mapToArray<T>(map: Map<T>): T[];

    /*@ arrayToMap :: forall T . (array: IArray<T>, makeKey: (value: T) => string) => { Map<Mutable,T> | true } */
    export declare function arrayToMap<T>(array: T[], makeKey: (value: T) => string): Map<T>;

    /*@ getLocaleSpecificMessage :: (message: string) => { string | true } */
    export declare function getLocaleSpecificMessage(message: string): string;
    //// TODO
 
    /*@ compareValues :: forall T . (a:T, b:T) => { number | true } */
    export declare function compareValues<T>(a: T, b: T): number;

    // PV: added object types with constructor signature instead of constructor func sig
    export interface ObjectAllocator {
        getNodeConstructor(kind: SyntaxKind): { new (): Node };
        getSymbolConstructor(): { 
          /*@ new (flags: SymbolFlags, name: string) => { v: ISymbol | offset(v, "flags") = flags } */
          new (flags: SymbolFlags, name: string): Symbol 
        };
        getTypeConstructor(): { new (checker: TypeChecker, flags: TypeFlags): Type };
        getSignatureConstructor(): { new (checker: TypeChecker): Signature };
    }
      
    /*@ newType :: forall M . (checker: TypeChecker<Immutable>, flags: bitvector32) => { Type<M> | type_flags(flags,v) } */
    export declare function newType(checker: TypeChecker, flags: TypeFlags): Type;

}
