
interface Object { }

// https://github.com/Microsoft/TypeScript/blob/master/lib/lib.d.ts#L82

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

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...in
// /*@ builtin_BIForInKeys ::
//     /\ forall A . (a: IArray<A>)                 => IArray<{ number | (0 <= v && v < (len a)) }>
//     /\            (o: Object<Immutable>)         => IArray<{ string | (hasProperty(v,o) && enumProp(v,o)) }>
//     /\            (o: [Immutable]{ })            => IArray<{ string | (hasProperty(v,o) && enumProp(v,o)) }>
//     /\ forall A . (o: [Immutable]{[s:string]:A}) => IArray<{ string | (hasProperty(v,o) && enumProp(v,o)) }>
//  */
// //TODO: remove the last overload once {[s:string]:A} extends { }
// declare function builtin_BIForInKeys(obj: Object): string[];

/*@ builtin_OpInstanceof :: <A>(x:A, s: string) => { v: boolean | Prop v <=> extends_class(x, s) } */
declare function builtin_OpInstanceof<A>(x: A, s: string): boolean;

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
// /*@ builtin_OpIn ::
//     /\ forall A . (i: number, a: IArray<A>) => { v: boolean | ((Prop v) <=> (0 <= i && i < (len a))) }
//     /\            (s: string, o: { }      ) => { v: boolean | ((Prop v) <=> hasProperty(s,o)) }
//  */
// declare function builtin_OpIn(s: string, obj: Object): boolean;
