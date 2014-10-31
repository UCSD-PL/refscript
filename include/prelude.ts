/*************************************************************************
 *        
 *  GENERAL PURPOSE AUXILIARY DEFINITIONS 
 *
 ************************************************************************/

/*@ crash :: forall A. () => A */
declare function crash(): any; 

/*@ assume :: (x:boolean) => {v:void | Prop x} */
declare function assume(x: boolean): void;

/*@ assert :: forall A . ({x:A|(Prop x)}) => void */
declare function assert<A>(x: A): void;

/*@ random :: () => {v:number | true} */
declare function random(): number;

/*@ pos :: () => {v:number | v > 0} */
declare function pos(): number;

declare function alert(s: string): void;

interface Pair<A,B> { x: A; y: B; }

/*@ isNaN :: (x:undefined + number) => {v:boolean | Prop v <=> (ttag(v) != "number")} */ 
declare function isNaN(x:any) : boolean;

/*************************************************************************
 *        
 *  TYPES FOR BUILTIN OPERATORS 
 *
 ************************************************************************/

/*@ builtin_BIBracketRef ::
    /\ forall A. (arr: #Array[#Immutable,A], {idx: number | (0 <= idx && idx < (len arr))}) => A
    /\ forall A. (arr: #Array[#Mutable, A ], idx: number) => A + undefined
    /\ forall A. (o: {[y: string]: A }, x: { string | keyIn(x,o) }) => A
 */
declare function builtin_BIBracketRef<A>(arr: A[], n: number): A;

/*@ builtin_BIBracketAssign :: 
    /\ forall A. (arr: #Array[#Immutable, A], {idx:number | (0 <= idx && idx < (len arr))}, val: A) => void
    /\ forall A. (arr: #Array[#ReadOnly , A], idx:number, val: A) => void
    /\ forall A M. ([#Mutable]{[y: string]: A }, x:string, val: A) => void
 */
declare function builtin_BIBracketAssign<A>(arr: A[], n: number, v: A): void;

/*@ builtin_BISetProp :: 
    forall A M. ([M] { f ? : [#Mutable] A }, A) => A 
 */
declare function builtin_BISetProp<A>(o: { f: A }, v: A): A;

/*@ builtin_BIArrayLit :: 
    forall M A. (A) => {v: #Array[M,A] | [ (len v) = builtin_BINumArgs; not (null v) ] } 
 */
declare function builtin_BIArrayLit<A>(a: A): A[];

/*@ builtin_BICondExpr :: 
    forall C T . (c: C, t: T, x: T, y: T) => { v: T | (if (Prop(c)) then (v ~~ x) else (v ~~ y)) } 
 */
declare function builtin_BICondExpr<C, T>(c: C, t: T, x: T, y: T): T;

declare function builtin_BICastExpr<T>(c: T, x: T): T;

/*@ builtin_OpLT :: 
    /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <  y)) }
    /\ forall T. (x:T, y:T) => boolean
 */
declare function builtin_OpLT(a: any, b: any): boolean;

/*@ builtin_OpLEq :: 
    /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <= y)) }
    /\ forall T. (x:T, y:T) => boolean
 */
declare function builtin_OpLEq(a: any, b: any): boolean;

/*@ builtin_OpGT :: 
    /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >  y)) }
    /\ forall T. (x:T, y:T) => boolean
 */
declare function builtin_OpGT(a: any, b: any): boolean;

/*@ builtin_OpGEq ::
    /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >= y)) }
    /\ forall T. (x:T, y:T) => boolean
 */
declare function builtin_OpGEq(a: any, b: any): boolean;

/*@ builtin_OpAdd :: 
    /\ (x:number, y:number) => {number | v = x + y}
    /\ (x:number, y:string) => string
    /\ (x:string, y:number) => string
    /\ (x:string, y:string) => string           
    /\ (x:{top|false}, y:{top|false}) => number + string
 */
declare function builtin_OpAdd(a: any, b: any): any;
// FIXME: what is the last line useful for?

/*@ builtin_OpSub :: 
    ({x:number | true}, {y:number | true})  => {v:number | v ~~ x - y} 
*/
declare function builtin_OpSub(a: number, b: number): number;

declare function builtin_OpMul(a: number, b: number): number;

/*@ builtin_OpDiv :: 
    (x: number, y: { v: number | v != 0 }) => { v:number | (((x>0 && y>0) => v>0) 
                                                        && (x=0 <=> v=0) 
                                                        && ((x>0 && y>1) => v<x) )} 
 */
declare function builtin_OpDiv(a: number, b: number): number;
// FIXME: This is not correct. Add definition for: >>

declare function builtin_OpMod(a: number, b: number): number;

/*@ builtin_PrefixPlus ::
    ({x:number  | true}) => {v:number  | v ~~ x}
 */
declare function builtin_PrefixPlus(a: number): number;

/*@ builtin_PrefixMinus :: 
    ({x:number  | true}) => {v:number  | v ~~ (0 - x)} 
 */
declare function builtin_PrefixMinus(a: number): number;

/*  builtin_OpEq :: 
    forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x ~~ y)) } 
 */
// declare function builtin_OpEq<A,B>(a: A, b: B): boolean;

/*  builtin_OpSEq :: 
    /\ forall A  . (x:A, y:A) => {v:boolean | ((Prop v) <=> (x = y)) }
    /\ forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x ~~ y)) } 
 */
/*@ builtin_OpSEq :: 
    forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x ~~ y)) } 
 */
declare function builtin_OpSEq<A,B>(x: A, y: B): boolean;

/*@ builtin_OpNEq :: 
    forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (not (x ~~ y))) } 
 */
declare function builtin_OpNEq<A,B>(x: A, y: B): boolean;

/*@ builtin_OpSNEq :: 
    forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (not (x ~~ y))) } 
 */
declare function builtin_OpSNEq<A,B>(x: A, y: B): boolean;
// FIXME: the two version of inequality should not be the same...

/*@ builtin_OpLAnd :: 
    /\ forall A. (x:A, y:A) => { v:A | (if (Prop(x)) then (v = y) else (v = x)) }
    /\ forall A B. (x:A, y:B) => { v:top | (Prop(v) <=> (Prop(x) && Prop(y))) }
 */
declare function builtin_OpLAnd(x: any, y: any): any;
      
/*@ builtin_OpLOr :: 
    /\ forall A. (x: undefined, y:A) => A
    /\ forall A. (x: null, y:A) => A
    /\ forall A. (x:A, y:A) => { v:A | ((Prop v) => (v ~~ y) && (not (Prop v) => (v ~~ x))) } 
    /\ forall A B. (x:A, y:B)  => { v:top | (Prop(v) <=> (Prop(x) || Prop(y))) }
 */
declare function builtin_OpLOr(x: any, y: any): any;

/*@ builtin_PrefixLNot :: 
    forall A. (x: A) => {v:boolean | (((Prop v) <=> not Prop(x)) && ((Prop v) <=> FLS(x)))} 
 */
declare function builtin_PrefixLNot<A>(x: A): boolean;

/*@ builtin_PrefixBNot ::
    (x: number) => {v:number | v = 0 - (x + 1) } 
 */
declare function builtin_PrefixBNot(n: number): number;

declare function builtin_OpBOr(a: number, b: number): number;
declare function builtin_OpBXor(a: number, b: number): number;
declare function builtin_OpBAnd(a: number, b: number): number;
/**
 *
 *    Bitwise "and" operator `&`
 *    
 *     builtin_OpBAnd :: (a: number, y: number) 
 *                     => { v: number | (bv_idx(a,1)  && bv_idx(b,1)  => bv_idx(v,1)) 
 *                                      (bv_idx(a,2)  && bv_idx(b,2)  => bv_idx(v,2))  
 *                                      ...
 *                                      (bv_idx(a,32) && bv_idx(b,32) => bv_idx(v,32)) 
 *                        } 
 *
 */


declare function builtin_OpLShift(a: number, b: number): number;
declare function builtin_OpSpRShift(a: number, b: number): number;
declare function builtin_OpZfRShift(a: number, b: number): number;


/**
 *
 *    for ... in ... 
 *
 *    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...in
 *
 *    A for...in loop only iterates over enumerable properties. Objects created from
 *    built–in constructors like Array and Object have inherited non–enumerable
 *    properties from Object.prototype and String.prototype, such as String's
 *    indexOf() method or Object's toString() method. The loop will iterate over all
 *    enumerable properties of the object itself and those the object inherits from
 *    its constructor's prototype (properties closer to the object in the prototype
 *    chain override prototypes' properties).
 *
 */

/*@ builtin_BIForInKeys :: 
    /\ forall A . (a: #Array[#Immutable, A]) => #Array[#Immutable, { v: number | (0 <= v && v < (len a)) }]
    /\ (o: [#Immutable]{ }) => #Array[#Immutable, { v: string | (keyIn(v,o) && enumProp(v,o)) }]
 */
declare function builtin_BIForInKeys(obj: Object): string[];



/*************************************************************************
 *
 *    OBJECT RELATED MEASURES
 *
 ************************************************************************/

/**
 *
 *    keyIn 
 *
 *    This property is true if the first string argument is an existing field 
 *    of either the object referenced in the second or the objects it inherits 
 *    from through its prototype chain.
 *
 */

/*@ measure keyIn      :: forall A . (string, A) => bool */

/*@ measure enumProp   :: forall A . (string, A) => bool */

/*@ measure keyVal     :: forall A B . (A,string) => B */



/*************************************************************************
 *
 *    AMBIENT DEFINITIONS 
 *
 *    Taken from here: 
 *
 *    http://typescript.codeplex.com/sourcecontrol/latest#typings/core.d.ts
 *
 *************************************************************************/


/**
 *  OBJECT 
 *
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L80
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L115
 *
 */

interface Object {
    // TODO
    /** The initial value of Object.prototype.constructor is the standard built-in Object constructor. */
    // constructor: Function;

    /** Returns a string representation of an object. */
    toString(): string;

    /** Returns a date converted to a string using the current locale. */
    toLocaleString(): string;

    /** Returns the primitive value of the specified object. */
    valueOf(): Object;

    /**
      * Determines whether an object has a property with the specified name. 
      * @param v A property name.
      */
    hasOwnProperty(v: string): boolean;

    /**
      * Determines whether an object exists in another object's prototype chain. 
      * @param v Another object whose prototype chain is to be checked.
      */
    isPrototypeOf<A>(v: A): boolean;

    /** 
      * Determines whether a specified property is enumerable.
      * @param v A property name.
      */
    propertyIsEnumerable(v: string): boolean;
}



declare var Object: {
    new <A>(value: A): Object;						// new (value?: any): Object;
    (): any;
    <A>(value: A): any;								// (value: any): any;

    prototype: Object;

    getPrototypeOf<A>(o: A): any;					// getPrototypeOf(o: any): any;

    // getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor;

    getOwnPropertyNames<A>(o: A): string[];			// getOwnPropertyNames(o: any): string[];

    // create(o: any, properties?: PropertyDescriptorMap): any;

    // defineProperty(o: any, p: string, attributes: PropertyDescriptor): any;

    // defineProperties(o: any, properties: PropertyDescriptorMap): any;

    // seal(o: any): any;

    // freeze(o: any): any;

    // preventExtensions(o: any): any;

    // isSealed(o: any): boolean;

    // isFrozen(o: any): boolean;

    // isExtensible(o: any): boolean;

    keys<A>(o: A): string[];						// keys(o: any): string[];
}



/**
 *  NUMBER 
 *
 * 
 *  TODO: 
 *
 *    - NaN =/= NaN
 *
 *    - all optional arguments have been changed to necessary
 *
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L430
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L457
 *
 */

/*@ measure numeric_nan               :: number */
/*@ measure numeric_max_value         :: number */
/*@ measure numeric_min_value         :: number */
/*@ measure numeric_negative_infinity :: number */
/*@ measure numeric_positive_infinity :: number */

/*@  NaN :: { number | v = numeric_nan } */
declare var NaN: number;


interface Number {
    toString(radix?: number): string;

    toFixed(fractionDigits?: number): string;

    toExponential(fractionDigits?: number): string;

    toPrecision(precision?: number): string;
}



declare var Number: {
    new (value: any): Number;						// new (value?: any): Number;
    <A>(value: A): number;							// (value?: any): number;
    prototype: Number;

    /*  MAX_VALUE: { number | v = numeric_max_value } */
    MAX_VALUE: number;

    /*  MIN_VALUE: { number | v = numeric_min_value } */
    MIN_VALUE: number;

    /*  NaN: { number | v = numeric_nan } */
    NaN: number;

    /*  NEGATIVE_INFINITY: { number | v = numeric_negative_infinity } */
    NEGATIVE_INFINITY: number;

    /*  POSITIVE_INFINITY: { number | v = numeric_positive_infinity } */
    POSITIVE_INFINITY: number;
}



/**
 *  MATH
 *
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L487
 *
 */

interface Math {
    E: number;
    LN10: number;
    LN2: number;
    LOG2E: number;
    LOG10E: number;
    PI: number;
    SQRT1_2: number;
    SQRT2: number;
    abs(x: number): number;
    acos(x: number): number;
    asin(x: number): number;
    atan(x: number): number;
    atan2(y: number, x: number): number;
    ceil(x: number): number;
    cos(x: number): number;
    exp(x: number): number;
    floor(x: number): number;
    log(x: number): number;
    // max(...values: number[]): number;
    // min(...values: number[]): number;
    pow(x: number, y: number): number;
    random(): number;
    round(x: number): number;
    sin(x: number): number;
    sqrt(x: number): number;
    tan(x: number): number;
}

declare var Math: Math;


/**
 *  STRING
 *  
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L259
 *
 */

interface String {
    toString(): string;

    charAt(pos: number): string;

    charCodeAt(index: number): number;

    // concat(...strings: string[]): string;

    indexOf(searchString: string, position?: number): number;

    lastIndexOf(searchString: string, position: number): number;

    localeCompare(that: string): number;

    match(regexp: string): string[];

    // match(regexp: RegExp): string[];

    replace(searchValue: string, replaceValue: string): string;

    // replace(searchValue: string, replaceValue: (substring: string, ...args: any[]) => string): string;

    // replace(searchValue: RegExp, replaceValue: string): string;

    // replace(searchValue: RegExp, replaceValue: (substring: string, ...args: any[]) => string): string;

    search(regexp: string): number;

    // search(regexp: RegExp): number;

    slice(start?: number, end?: number): string;

    split(separator: string, limit?: number): string[];

    // split(separator: RegExp, limit?: number): string[];

    substring(start: number, end?: number): string;

    toLowerCase(): string;

    toLocaleLowerCase(): string;

    toUpperCase(): string;

    toLocaleUpperCase(): string;

    trim(): string;

    /*@ length: { number | v >= 0 } */
    length: number;

    substr(from: number, length?: number): string;

    // [index: number]: string;
}

declare var String: {
    new (value?: any): String;
    (value?: any): string;
    prototype: String;
    // fromCharCode(...codes: number[]): string;
}

interface Boolean { }


/**
 *  ARRAY 
 *  
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L966
 *
 *    TODO: Fix mutabilities
 *    consult: https://github.com/UCSD-PL/RefScript/blob/develop/include/prelude.ts
 *
 */
 

/*@ measure len :: forall A . (A) => number */


/*@ interface Array<M, T> */
interface Array<T> {

    toString(): string;

    toLocaleString(): string;

    /*@ concat: 
        /\ forall M0 . (this: #Array[#Immutable,T], items: #Array[#Immutable,T]): { #Array[M0,T] | (len v) = (len this) + (len items) }
        /\ forall M0 M1 M2 . (this: M0, items: #Array[M1,T]): #Array[M2,T]
    */
    concat<U extends T[]>(...items: U[]): T[];

    // concat(...items: T[]): T[];
  
    join(separator?: string): string;

    /*@ pop: (this: #Array[#Mutable, T]): T */
    pop(): T;

    /*@ push: (this: #Array[#Mutable,T], items: T): number */
    push(T): number;								// push(...items: T[]): number;

    /*@ reverse: (this: #Array[M,T]): #Array[M,T] */
    reverse(): T[];

    shift(): T;

    /*@ slice: forall N . (this: #Array[M,T], start: number, start: number): #Array[N,T] */
    slice(start/*?*/: number, end/*?*/: number): T[];

    /*@ sort : 
        /\ ( ) => { v : Array<M,T> | len(v) = len(this) } 
        /\ (compareFn: (a: T, b: T) => number) => { v : Array<M,T> | len(v) = len(this) } 
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

    /*@ map : forall U. (callbackfn: (value: T) => U) => { IArray<U> | true} */
    map<U>(callbackfn: (value: T) => U): U[];
    
    /*@ map : forall U. (callbackfn:(value: T, index: number) => U)=> { IArray<U> | true} */
    map<U>(callbackfn: (value: T, index: number) => U): U[];
    
    /*@ map : forall U. (callbackfn:(value: T, index: number, array: IArray<T>) => U) => { IArray<U> | true } */
    map<U>(callbackfn: (value: T, index: number, array: T[]) => U): U[];

    /*@ filter : 
        /\ forall N . (callbackfn: (value: T) => boolean) => { Array[N, T] | true }
        /\ forall N . (callbackfn: (value: T, index: number) => boolean) => { Array[N, T] | true }
        /\ forall N . (callbackfn: (value: T, index: number, array: IArray<T>) => boolean) => { Array[N, T] | true } */
    filter(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): T[];


    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;

    /*@ reduce : forall U . (this: Array<Immutable, T>, callback: (x: U, y: T, n: {number | 0 <= v && v < len this}) => U, init: U) => U */
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: T): T;
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    /*@ length: { v: number | (v = (len this) && v >= 0) } */
    length: number;

//      // [n: number]: T;
}

declare var Array: {

    /*@ forall M T . () => { v: #Array[M, T] | [ (len v) = 0; (not (null v))] } */
    (): any[];

    /*@ forall M T. (arrayLength: number) => { v: #Array[M, T] | [ (len v) = arrayLength; (not (null v))] } */
    <T>(arrayLength: number): T[];

    /*@ new forall M T . () => { v: #Array[M, T] | [ (len v) = 0; (not (null v))] } */
    new (): any[];

    /*@ new forall M T. (arrayLength: number) => { v: #Array[M, T] | [ (len v) = arrayLength; (not (null v))] } */
    new <T>(arrayLength: number): T[];

    // new <T>(...items: T[]): T[];

    
    // <T>(...items: T[]): T[];

    /*@ isArray: 
        /\ forall M T. (arg: #Array[M,T]): { v: boolean | Prop(v) }
        /\ forall A . (arg: A): boolean
    */
    isArray(arg: any): boolean;

    prototype: Array<any>;
}


interface IArguments {
    [index: number]: any;
    length: number;
    // callee: Function;
}


/*** Function ************************************************************/

interface Function {
    /**
      * Calls the function, substituting the specified object for the this 
      * value of the function, and the specified array for the arguments of the function.
      * @param thisArg The object to be used as the this object.
      * @param argArray A set of arguments to be passed to the function.
      */
    apply(thisArg: any, argArray?: any): any;

    /**
      * Calls a method of an object, substituting another object for the current object.
      * @param thisArg The object to be used as the current object.
      * @param argArray A list of arguments to be passed to the method.
      */
    call(thisArg: any, ...argArray: any[]): any;

    /**
      * For a given function, creates a bound function that has the same body 
      * as the original function. 
      * The this object of the bound function is associated with the specified 
      * object, and has the specified initial parameters.
      * @param thisArg An object to which the this keyword can refer inside the new function.
      * @param argArray A list of arguments to be passed to the new function.
      */
    bind(thisArg: any, ...argArray: any[]): any;

    prototype: any;
    length: number;

    // Non-standard extensions
    arguments: any;
    caller: Function;
}

declare var Function: {
    /** 
      * Creates a new function.
      * @param args A list of arguments the function accepts.
      */
    //new (...args: string[]): Function;
    //(...args: string[]): Function;
    prototype: Function;
}


/*** Console ************************************************************/
interface Console {
    info(message?: any, ...optionalParams: any[]): void;
    warn(message?: any, ...optionalParams: any[]): void;
    error(message?: any, ...optionalParams: any[]): void;
    log(message?: any, ...optionalParams: any[]): void;
    profile(reportName?: string): void;
    assert(test?: boolean, message?: string, ...optionalParams: any[]): void;
    //msIsIndependentlyComposed(element: Element): boolean;
    clear(): void;
    dir(value?: any, ...optionalParams: any[]): void;
    profileEnd(): void;
    count(countTitle?: string): void;
    groupEnd(): void;
    time(timerName?: string): void;
    timeEnd(timerName?: string): void;
    trace(): void;
    group(groupTitle?: string): void;
    dirxml(value: any): void;
    debug(message?: string, ...optionalParams: any[]): void;
    groupCollapsed(groupTitle?: string): void;
    //select(element: Element): void;
}
declare var Console: {
    prototype: Console;
    new(): Console;
}
declare var console: Console;



/*************************************************************************
 *       
 *          RUN-TIME TAGS 
 * 
 ************************************************************************/



/*@ measure ttag :: forall A . (A) => string */

/*@ measure FLS  :: forall A . (A) => bool */

/*@ measure Prop :: forall A . (A) => bool */

/*@ measure null :: forall A . (A) => bool */

/*@ builtin_PrefixTypeof :: 
    forall A. (x:A) => {v:string | (ttag x) = v }                
 */
declare function builtin_PrefixTypeof<A>(x: A): string; 

/*@ builtin_BITruthy :: 
    forall A. (x:A) => { v:boolean | ((Prop v) <=> Prop(x)) }        
*/
declare function builtin_BITruthy<A>(x: A): boolean; 

/*@ builtin_BIFalsy :: 
    forall A. (x:A) => { v:boolean | ((Prop v) <=> FLS(x)) }          
*/
declare function builtin_BIFalsy<A>(x: A): boolean; 

/*@ invariant {v:undefined | [(ttag(v) = "undefined"); not (Prop(v))]} */

/*@ invariant {v:null | [(ttag(v) = "object"); not (Prop(v)); null(v) ]} */

/*@ invariant {v:boolean | [(ttag(v) = "boolean")]} */ 

/*@ invariant {v:string | [(ttag(v) = "string"); (Prop(v) <=> v /= "" )]} */

/*@ invariant {v:number | [(ttag(v)  =  "number");
                           (Prop(v) <=> v /= 0  ); 
                           (FLS(v)  <=> v  = 0  )]}	*/


/**
 *
 *    Encoding of bitvector behavior
 *
 */

/*@ measure bv_idx :: (number, number) => bool */

/*@ builtin_bitVector :: (n: number, i: number) => { number | bv_idx(n,i) } */
declare function builtin_bitVector(n: number, i: number): number;


/**
 *
 *    ... `instaneof` ... 
 *
 */

/*@ measure instanceof :: forall A . (A,string) => bool */

/*@ builtin_OpInstanceof :: 
    forall A . (x:A, s: string) => { v: boolean | (Prop(v) <=> instanceof(x,s)) }     
*/
declare function builtin_OpInstanceof<A>(x: A, s: string): boolean; 


/**
 *
 *    ... `in` ... 
 *
 *   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in 
 * 
 *   The in operator returns true for properties in the prototype chain.
 *
 */

/*@ builtin_OpIn :: 
    /\ forall A . (i: number, a: #Array[#Immutable,A]) => { v: boolean | ((Prop v) <=> (0 <= i && i < (len a))) }
    /\ (s: string, o: { }) => { v: boolean | ((Prop v) <=> keyIn(s,o)) } 
 */
declare function builtin_OpIn(s: string, obj: Object): boolean;




/*************************************************************************
 *        
 *        Type Aliases
 * 
 ************************************************************************/

/*@ alias IArray<T> = Array<Immutable, T> */


/*************************************************************************
 *        
 *        PRE-LOADED QUALIFIERS 
 * 
 ************************************************************************/

/*@ qualif Bot(v:a): 0 = 1 */
/*@ qualif Bot(v:obj): 0 = 1 */
/*@ qualif Bot(v:boolean): 0 = 1 */
/*@ qualif Bot(v:number): 0 = 1 */
/*@ qualif CmpZ(v:number): v < 0 */
/*@ qualif CmpZ(v:number): v <= 0 */
/*@ qualif CmpZ(v:number): v >  0 */
/*@ qualif CmpO(v:number): v >  1 */
/*@ qualif CmpZ(v:number): v >= 0 */
/*@ qualif CmpZ(v:number): v =  0 */
/*@ qualif CmpZ(v:number): v != 0 */

/*@ qualif Cmp(v:number, x:number): v <  x */
/*@ qualif Cmp(v:number, x:number): v <= x */
/*@ qualif Cmp(v:number, x:number): v >  x */
/*@ qualif Cmp(v:number, x:number): v >= x */

/*@ qualif Cmp(v:a,x:a): v =  x */
/*@ qualif Cmp(v:a,x:a): v != x */
/*@ qualif One(v:number): v = 1 */
/*  qualif True(v:boolean): (v) */
/*  qualif False(v:boolean): (not v) */
/*@ qualif True1(v:boolean): (Prop v) */
/*@ qualif False1(v:boolean): not (Prop v) */

/*@ qualif Tag(v:a,x:string): ttag(v) = x */


// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*  qualif Add(v:number,x:number,y:number): v = x + y */
/*  qualif Sub(v:number,x:number,y:number): v = x - y */

/*@  qualif Len(v:b, w:a)  : v < (len w) */



/*************************************************************************
 *        
 *        ERROR HANDLING
 *
 ************************************************************************/

// NOTE: types that are defined in lib.d.ts need to be in comment to pass
// through the TS compilation phase.

interface Error {
    name: string; 
    message: string;
}

declare var Error: {
    new (message?: string): Error;
    (message?: string): Error;
    prototype: Error;
}



/*************************************************************************
 *
 *      MUTABILITY 
 *    
 *      Do not include type parameters here 
 *    
 ************************************************************************/

/*@ interface ReadOnly */
interface ReadOnly { }

/*@ interface Immutable extends ReadOnly */
interface Immutable extends ReadOnly {
    immutable__: void;
} 

/*@ interface Mutable extends AssignsFields */
interface Mutable extends AssignsFields {
    mutable__: void;
} 

/*@ interface AnyMutability extends ReadOnly */
interface AnyMutability extends ReadOnly {
    defaultMut__: void;
} 

/*@ interface AssignsFields extends ReadOnly */
interface AssignsFields extends ReadOnly {
    assignsFields_: void;
} 

/*@ interface InheritedMut */
interface InheritedMut {
    inheritedMut__: void;
} 


/*************************************************************************
 *
 *      OPTIONAL FIELDS
 *    
 *      Do not include type parameters here !!!
 *    
 ************************************************************************/

/*@ interface RequiredField extends OptionalField */
interface RequiredField extends OptionalField {
  requiredField__: void;
}

/*@ interface OptionalField */
interface OptionalField {
  optionalField__: void;
}

