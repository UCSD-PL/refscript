/*************************************************************************
        
  | General purpose auxiliary definitions 

*************************************************************************/

declare function crash<A>(): A; 

declare function assume(x: boolean): void;

/*@ assert :: ({x:boolean|(Prop x)}) => void */
declare function assert(x: boolean): void;

/*@ random :: () => {v:number | true} */
declare function random(): number;

/*@ pos :: () => {v:number | v > 0} */
declare function pos(): number;

declare function alert(s: string): void;

interface Pair<A,B> { x: A; y: B; }



/*************************************************************************
        
  | Types for Builtin Operators 

*************************************************************************/

/*@ builtin_BIBracketRef ::
    /\ forall A. (arr: #Array[#Immutable,A], {idx: number | (0 <= idx && idx < (len arr))}) => A
    /\ forall A. (arr: #Array[#Mutable, A ], idx: number) => A + undefined
    /\ forall A. (o: {[y: string]: A }, x: string) => { A | keyIn(x,o)} + { undefined | not (keyIn(x,o)) }
*/
declare function builtin_BIBracketRef<A>(arr: A[], n: number): A;

/*@ builtin_BIBracketAssign :: 
    /\ forall A. (arr: #Array[#Immutable, A], {idx:number | (0 <= idx && idx < (len arr))}, val: A) => void
    /\ forall A. (arr: #Array[#ReadOnly , A], idx:number, val: A) => void
    /\ forall A M. ([#Mutable]{[y: string]: A }, x:string, val: A) => void
*/
declare function builtin_BIBracketAssign<A>(arr: A[], n: number, v: A): void;

/*  builtin_BISetProp ::
    /\ forall A M. ([M] { f : [#Mutable] A }, A) => A
    /\ forall A M. ([#Mutable] { f : [M] A }, A) => A
*/

/*@ builtin_BISetProp :: 
    forall A M. ([M] { f : [#Mutable] A }, A) => A 
 */
declare function builtin_BISetProp<A>(o: { f: A }, v: A): A;

/*@ builtin_BIArrayLit :: 
    forall M A. (A) => {v: #Array[M,A] | (len v) = builtin_BINumArgs } 
*/
declare function builtin_BIArrayLit<A>(a: A): A[];

/*@ builtin_BICondExpr :: 
    forall A. (c: boolean, x: A, y: A) => { v:A | (if (Prop(c)) then (v = x) else (v = y)) }
*/
declare function builtin_BICondExpr<A>(c: boolean, x: A, y: A): A;

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
    /\ (x:{top|false}, y:{top|false}) => top                          
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
    /\ forall A. (x:A, y:A) => { v:A | (if (FLS(x)) then (v = y) else (v = x)) } 
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


// 
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...in
//
/*@ builtin_BIForInKeys :: 
    /\ forall A . (a: #Array[#Immutable, A]) => #Array[#Immutable, { v: number | (0 <= v && v < (len a)) }]
    /\ (o: [#Immutable]{ }) => #Array[#Immutable, { v: string | (keyIn(v,o) && enumProp(v,o)) }]
 */
declare function builtin_BIForInKeys(obj: Object): string[];



/*************************************************************************

  | Object related measures

*************************************************************************/

/*@ measure keyIn      :: forall A . (string, A) => bool */
/*@ measure enumProp   :: forall A . (string, A) => bool */



/*************************************************************************
  
  | Ambient Definitions 

  Taken from here: 

  http://typescript.codeplex.com/sourcecontrol/latest#typings/core.d.ts

**************************************************************************/


/*** Object **************************************************************/

// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L80

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


// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L115

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



/*** Number **************************************************************/

// FIXME:	NaN =/= NaN

/*@ measure numeric_nan               :: number */
/*@ measure numeric_max_value         :: number */
/*@ measure numeric_min_value         :: number */
/*@ measure numeric_negative_infinity :: number */
/*@ measure numeric_positive_infinity :: number */


/*@  NaN :: { number | v = numeric_nan } */
declare var NaN: number;


// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L430
// TODO: all optional arguments have been changed to necessary
interface Number {
    toString(radix/*?*/: number): string;

    toFixed(fractionDigits/*?*/: number): string;

    toExponential(fractionDigits/*?*/: number): string;

    toPrecision(precision/*?*/: number): string;
}


// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L457

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



/*** Math ****************************************************************/

// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L487

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



/*** String **************************************************************/

// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L259


interface String {
    toString(): string;

    charAt(pos: number): string;

    charCodeAt(index: number): number;

    // concat(...strings: string[]): string;

    indexOf(searchString: string, position/*?*/: number): number;

    lastIndexOf(searchString: string, position/*?*/: number): number;

    localeCompare(that: string): number;

    match(regexp: string): string[];

    // match(regexp: RegExp): string[];

    replace(searchValue: string, replaceValue: string): string;

    // replace(searchValue: string, replaceValue: (substring: string, ...args: any[]) => string): string;

    // replace(searchValue: RegExp, replaceValue: string): string;

    // replace(searchValue: RegExp, replaceValue: (substring: string, ...args: any[]) => string): string;

    search(regexp: string): number;

    // search(regexp: RegExp): number;

    slice(start/*?*/: number, end/*?*/: number): string;

    split(separator: string, limit/*?*/: number): string[];

    // split(separator: RegExp, limit/*?*/: number): string[];

    substring(start: number, end/*?*/: number): string;

    toLowerCase(): string;

    toLocaleLowerCase(): string;

    toUpperCase(): string;

    toLocaleUpperCase(): string;

    trim(): string;

    /*@ length: { number | v >= 0 } */
    length: number;

    substr(from: number, length/*?*/: number): string;

    // [index: number]: string;
}

declare var String: {
    new (value/*?*/: any): String;
    (value?: any): string;
    prototype: String;
    // fromCharCode(...codes: number[]): string;
}



/*** Array ***************************************************************/

/*@ measure len      :: forall M A . (#Array[M,A]) => number             */

// https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L966

// TODO: Fix mutabilities
// consult: https://github.com/UCSD-PL/RefScript/blob/develop/include/prelude.ts

/*@ interface Array<M, T> */
interface Array<T> {

    toString(): string;

    toLocaleString(): string;

    /*@ concat: 
        /\ forall M0       . (this: #Array[#Immutable,T], items: #Array[#Immutable,T]): { #Array[M0,T] | (len v) = (len this) + (len items) }
        /\ forall M0 M1 M2 . (this: M0, items: #Array[M1,T]): #Array[M2,T]
    */
    concat<U extends T[]>(...items: U[]): T[];
    // concat(...items: T[]): T[];
  
    join(separator/*?*/: string): string;

    /*@ pop: (this: #Array[#Mutable, T]): T */
    pop(): T;

    /*@ push: (this: #Array[#Mutable,T], items: T): number */
    push(T): number;								// push(...items: T[]): number;

    /*@ reverse: (this: #Array[M,T]): #Array[M,T] */
    reverse(): T[];

    shift(): T;

    /*@ slice: forall N . (this: #Array[M,T], start: number, start: number): #Array[N,T] */
    slice(start/*?*/: number, end/*?*/: number): T[];

    sort(compareFn/*?*/: (a: T, b: T) => number): T[];

    splice(start: number): T[];

    // splice(start: number, deleteCount: number, ...items: T[]): T[];

    // unshift(...items: T[]): number;

    indexOf(searchElement: T, fromIndex/*?*/: number): number;

    lastIndexOf(searchElement: T, fromIndex/*?*/: number): number;

    every(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): boolean;

    some(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): boolean;

    forEach(callbackfn: (value: T, index: number, array: T[]) => void/*, thisArg?: any*/): void;

    map<U>(callbackfn: (value: T, index: number, array: T[]) => U/*, thisArg?: any*/): U[];

    filter(callbackfn: (value: T, index: number, array: T[]) => boolean/*, thisArg?: any*/): T[];

    reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue/*?*/: T): T;
    // reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue/*?*/: T): T;
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    /*@ length: { v: number | (v = (len this) && v >= 0) } */
    length: number;

//      // [n: number]: T;
}

declare var Array: {

    (arrayLength/*?*/: number): any[];

    /*@ new forall M T . (arrayLength: number) => { v: #Array[M, T] | [ (len v) = arrayLength; (not (null v))] } */
    new (arrayLength/*?*/: number): any[];

    /*@ new forall M T. (arrayLength: number) => { v: #Array[M, T] | [ (len v) = arrayLength; (not (null v))] } */
    new <T>(arrayLength: number): T[];

    // new <T>(...items: T[]): T[];


    <T>(arrayLength: number): T[];
    
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

/**
  * Creates a new function.
  */
interface Function {
    /**
      * Calls the function, substituting the specified object for the this value of the function, and the specified array for the arguments of the function.
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
      * For a given function, creates a bound function that has the same body as the original function. 
      * The this object of the bound function is associated with the specified object, and has the specified initial parameters.
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



/*************************************************************************
        
  | Run-Time Tags 

*************************************************************************/

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


/*@ measure instanceof :: forall A . (A,string) => bool */

/*@ builtin_OpInstanceof :: 
    forall A . (x:A, s: string) => { v: boolean | (Prop(v) <=> instanceof(x,s)) }     
*/
declare function builtin_OpInstanceof<A>(x: A, s: string): boolean; 



/*************************************************************************
        
  | Pre-Loaded Qualifiers 

*************************************************************************/

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


// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*  qualif Add(v:number,x:number,y:number): v = x + y */
/*  qualif Sub(v:number,x:number,y:number): v = x - y */

/*  qualif Len(v:number, n: number)  : n < (len v) */



/*************************************************************************
        
  | Error Handling

*************************************************************************/

// NOTE: types that are defined in lib.d.ts need to be in comment to pass
// through the TS compilation phase.

interface Error {
    name: string; 
    message: string;
}

declare var Error: {
    new (message/*?*/: string): Error;
    (message/*?*/: string): Error;
    prototype: Error;
}



//class Errors {

//	public static argument(argument: string, message/*?*/: string): Error {
//		return new Error("Invalid argument: " + argument + ". " + message);
//	}

//	public static argumentOutOfRange(argument: string): Error {
//		return new Error("Argument out of range: " + argument);
//	}

//	public static argumentNull(argument: string): Error {
//		return new Error("Argument null: " + argument);
//	}

//	public static abstract(): Error {
//		return new Error("Operation not implemented properly by subclass.");
//	}

//	public static notYetImplemented(): Error {
//		return new Error("Not yet implemented.");
//	}

//	public static invalidOperation(message/*?*/: string): Error {
//		return new Error("Invalid operation: " + message);
//	}
    
//}



/*************************************************************************
        
  | Mutability 

  Do not include type parameters here 

*************************************************************************/

/*@ interface ReadOnly */
interface ReadOnly { }

/*@ interface Immutable extends #ReadOnly */
interface Immutable extends ReadOnly {
    immutable__: void;
} 

/*@ interface Mutable extends #AssignsFields */
interface Mutable extends AssignsFields {
    mutable__: void;
} 

/*@ interface AnyMutability extends #ReadOnly */
interface AnyMutability extends ReadOnly {
    defaultMut__: void;
} 

/*@ interface AssignsFields extends #ReadOnly */
interface AssignsFields extends ReadOnly {
    defaultMut__: void;
} 




/*************************************************************************
        
  | DOM API 

*************************************************************************/

interface Event {
    timeStamp: number;
    defaultPrevented: boolean;
    isTrusted: boolean;
//    currentTarget: EventTarget;
    cancelBubble: boolean;
//    target: EventTarget;
    eventPhase: number;
    cancelable: boolean;
    type: string;
//    srcElement: Element;
    bubbles: boolean;
    initEvent(eventTypeArg: string, canBubbleArg: boolean, cancelableArg: boolean): void;
    stopPropagation(): void;
    stopImmediatePropagation(): void;
    preventDefault(): void;
    CAPTURING_PHASE: number;
    AT_TARGET: number;
    BUBBLING_PHASE: number;
}

declare var Event: {
    prototype: Event;
    new(): Event;
    CAPTURING_PHASE: number;
    AT_TARGET: number;
    BUBBLING_PHASE: number;
}

