
/**
    https://github.com/Microsoft/TypeScript/blob/master/lib/lib.d.ts#L94
*/

interface Object {
    hasOwnProperty(v: string): boolean;
    toString(): string;
}

interface ObjectConstructor<M extends ReadOnly> {
    new (value?: any): Object;
    (): any;
    /*@ (value: string): string */
    (value: any): any;
    prototype: Object;
    /*@ getPrototypeOf(o: string): { string | v = "" }  */
    getPrototypeOf(o: any): any;
    getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor<M>;
    getOwnPropertyNames(o: any): string[];
    create(o: any, properties?: PropertyDescriptorMap<M>): any;
    defineProperty(o: any, p: string, attributes: PropertyDescriptor<M>): any;
    defineProperties(o: any, properties: PropertyDescriptorMap<M>): any;
    seal<T>(o: T): T;
    freeze<T>(o: T): T;
    preventExtensions<T>(o: T): T;
    isSealed(o: any): boolean;
    isFrozen(o: any): boolean;
    isExtensible(o: any): boolean;
    keys(o: any): string[];
}

declare let Object: ObjectConstructor<ReadOnly>;


interface PropertyDescriptor<M extends ReadOnly> {
    configurable?: boolean;
    enumerable?: boolean;
    value?: any;
    writable?: boolean;
    get?(): any;
    set?(v: any): void;
}

interface PropertyDescriptorMap<M extends ReadOnly> {
    [s: string]: PropertyDescriptor<ReadOnly>;
}
