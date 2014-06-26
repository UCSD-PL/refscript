/*************************************************************************/
/*********** General purpose auxiliary functions *************************/
/*************************************************************************/

/*@ extern crash     :: forall A.()            => A                  */

/*@ extern assume    :: (boolean)              => void               */

/*@ extern assert    :: ({x:boolean|(Prop x)}) => void               */

/*@ extern requires  :: (boolean)              => void               */

/*@ extern ensures   :: (boolean)              => void               */

/*@ extern random    :: ()                     => number             */

/*@ extern pos       :: ()                     => {v:number | v > 0} */

/*@ extern alert     :: (string)               => void               */



/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/

/*@ extern builtin_BIBracketRef     :: 
    /\ forall A. (arr: #Array[#Immutable,A]   , {idx:number | (0 <= idx && idx < (len arr))}) => A 
    /\ forall A. (arr: #Array[#Mutable, A ]   , idx:number                                  ) => A? 
    /\ forall A. ([#ReadOnly]{[y: string]: A }, x:string                                    ) => A      
*/

/*@ extern builtin_BIBracketAssign  :: 
    /\ forall A  . (arr: #Array[#Immutable, A] , {idx:number | (0 <= idx && idx < (len arr))}, val: A) => void
    /\ forall A  . (arr: #Array[#ReadOnly , A] ,  idx:number                                 , val: A) => void
    /\ forall A M. ([#Mutable]{[y: string]: A },    x:string                                 , val: A) => void  
*/


/*@ extern builtin_BISetProp ::
    /\ forall A M . ([M]        { f : [#Mutable] A }, A) => A
    /\ forall A M . ([#Mutable] { f : [M]        A }, A) => A
*/


//FIXME: the 'len' property is invalid if M != Immutable
/*@ extern builtin_BIArrayLit  :: forall M  A . (A) 
                               => {v: #Array[M,A] | [ (len v) = builtin_BINumArgs; not (null v) ] } 
*/

/*@ extern builtin_BIUndefined :: forall A. {A | false} */


/*@ extern builtin_OpLT        :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <  y)) }
                                  /\ (x:string, y:number) => boolean
                                  /\ (x:number, y:number) => boolean
                                  /\ (x:string, y:string) => boolean                                
*/

/*@ extern builtin_OpLEq       :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <= y)) } 
                                  /\ (x:string, y:number) => boolean
                                  /\ (x:number, y:number) => boolean
                                  /\ (x:string, y:string) => boolean                                
*/

/*@ extern builtin_OpGT        :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >  y)) } */

/*@ extern builtin_OpGEq       :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >= y)) } */

/*@ extern builtin_OpAdd       :: /\ (x:number, y:number) => {number | v = x + y}
                                  /\ (x:number, y:string) => string
                                  /\ (x:string, y:number) => string
                                  /\ (x:string, y:string) => string           
                                  /\ (x:{top|false}, y:{top|false}) => top                          
 */

/*@ extern builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v ~~ x - y} */

/*@ extern builtin_OpMul       :: (number,  number)  => number */

//FIXME: This is not correct. Add definition for: >>  
/*@ extern builtin_OpDiv       :: (x: number, y: { v: number | v != 0 }) 
                               => { v:number | (((x>0 && y>0) => v>0) && (x=0 <=> v=0) && ((x>0 && y>1) => v<x) )} 
 */

/*@ extern builtin_OpMod       :: (number,  number)  => number */

/*@ extern builtin_PrefixMinus :: ({x:number  | true}) => {v:number  | v ~~ (0 - x)} */

/*  extern builtin_OpEq        :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x ~~ y)) } */

/*@ extern builtin_OpSEq       :: /\ forall A  . (x:A, y:A) => {v:boolean | ((Prop v) <=> (x = y)) }
                                  /\ forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x ~~ y)) } */

/*@ extern builtin_OpNEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) } */

/*@ extern builtin_OpSNEq      :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) } */

// FIXME: the two version of inequality should not be the same...

/*@ extern builtin_OpLAnd      :: /\ forall A   . (x:A, y:A)  => { v:A   | (if (Prop(x)) then (v = y) else (v = x)) }
                                  /\ forall A B . (x:A, y:B)  => { v:top | (Prop(v) <=> (Prop(x) && Prop(y))) } 
*/
      
/*@ extern builtin_OpLOr       :: /\ forall A   . (x:A, y:A)  => { v:A   | (if (FLS(x)) then (v = y) else (v = x)) } 
                                  /\ forall A B . (x:A, y:B)  => { v:top | (Prop(v) <=> (Prop(x) || Prop(y))) }
*/

/*@ extern builtin_PrefixLNot  :: forall A . (x: A) => {v:boolean | (((Prop v) <=> not Prop(x)) && ((Prop v) <=> FLS(x)))} */

/*@ extern builtin_PrefixBNot  :: (x: number) => {v:number | v = 0 - (x + 1) } */




/*************************************************************************
  
  Ambient Definitions 

  Taken from here: 

  http://typescript.codeplex.com/sourcecontrol/latest#typings/core.d.ts

**************************************************************************/


/*** Object **************************************************************/


/*@ interface Object<M> {

      toString             : () => string;
      toLocaleString       : () => string;
      valueOf              : () => #Object;
      hasOwnProperty       : (v: string) => boolean;
      isPrototypeOf        : forall A . (v: A) => boolean;
      propertyIsEnumerable : (v: string) => boolean;

    } */

      ////TODO:
      //constructor: Function;



/*@ extern Object :: {

      new forall A . (value: A) => #Object;
      forall A .     (value: A) => top;

      prototype           : #Object;
      getPrototypeOf      : forall A . (o: A): top;
      getOwnPropertyNames : forall A . (o: A): #Array[#Immutable,string];
      keys                : forall A . (o: A): #Array[#Immutable,string];

    } */

      ////TODO: 
      //(): any;
      //getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor;
      //create(o: any, properties?: PropertyDescriptorMap): any;
      //defineProperty(o: any, p: string, attributes: PropertyDescriptor): any;
      //defineProperties(o: any, properties: PropertyDescriptorMap): any;
      //seal(o: any): any;
      //freeze(o: any): any;
      //preventExtensions(o: any): any;
      //isSealed(o: any): boolean;
      //isFrozen(o: any): boolean;
      //isExtensible(o: any): boolean;



/*** Number **************************************************************/

// TODO: create special constant values for NaN, MIN_VALUE, etc...

/*@ extern Number :: { 
      new forall A . (value: A) => #Number;
      forall A . (value: A) => number;

      prototype         : #Number;
      MAX_VALUE         : number;
      MIN_VALUE         : number;
      NaN               : number;
      NEGATIVE_INFINITY : number;
      POSITIVE_INFINITY : number;
    } */


/*@ interface Number {
      toString        : (radix: number):string;
      toString        : ( ): string;
      toFixed         : (fractionDigits: number) => string;
      toExponential   : (fractionDigits: number) => string;
      toPrecision     : (precision: number) => string
    } */



/*** Math ****************************************************************/

/*@ extern Math :: {
      E       : number;
      LN10    : number;
      LN2     : number;
      LOG2E   : number;
      LOG10E  : number;
      PI      : number;
      SQRT1_2 : number;
      SQRT2   : number;
      abs     : (x: number) => number;
      acos    : (x: number) => number;
      asin    : (x: number) => number;
      atan    : (x: number) => number;
      atan2   : (y: number, x: number) =>  number;
      ceil    : (x: number) => number;
      cos     : (x: number) => number;
      exp     : (x: number) => number;
      floor   : (x: number) => number;
      log     : (x: number) => number;
      max     : (values: #Array[#ReadOnly,number]) => number;
      min     : (values: #Array[#ReadOnly,number]) => number;
      pow     : (x: number, y: number) => number;
      random  : () =>  number;
      round   : (x: number) => number;
      sin     : (x: number) => number;
      sqrt    : (x: number) => number;
      tan     : (x: number) => number
    } */



/*** String **************************************************************/

/*@ extern String     :: {

      toString          : (): string;
      charAt            : (pos: number): string;
      charCodeAt        : (index: number): number;
      concat            : (strings: #Array[#ReadOnly,string]): string;
      indexOf           : (searchString: string, position: number): number;
      lastIndexOf       : (searchString: string, position: number): number;
      localeCompare     : (that: string): number;
      match             : forall M . (regexp: string): #Array[M, string];
      replace           : (searchValue: string, replaceValue: string): string;
      search            : (regexp: string): number;
      slice             : (start: number, end: number): string;
      split             : forall M . (separator: string, limit: number): #Array[M, string];
      substring         : (start: number, end: number): string;
      toLowerCase       : (): string;
      toLocaleLowerCase : (): string;
      toUpperCase       : (): string;
      toLocaleUpperCase : (): string;
      trim              : (): string;

      length            : { number | v >= 0 };

      substr            : (from: number, length: number) => string

    } */



/*** Array ***************************************************************/

/*@ measure len      :: forall M A . (#Array[M,A]) => number             */

/*@ interface Array<M,T> {
      toString       : (): string;
      toLocaleString : (): string;
      concat         : /\ forall M0       . (this: #Array[#Immutable,T], items: #Array[#Immutable,T]): { #Array[M0,T] | (len v) = (len this) + (len items) }
                       /\ forall M0 M1 M2 . (this: M0, items: #Array[M1,T]): #Array[M2,T];
      join           : (separator: string): string;
      pop            : (this: #Array[#Mutable, T]): T;
      push           : (this: #Array[#Mutable,T], items: T): number;
      reverse        : (this: #Array[M,T]): #Array[M,T];
      shift          : (): T;
      slice          : /\ forall N . (start: number): #Array[N,T]
                       /\ forall N . (start: number, end: number): #Array[N,T];
      sort           : (this: #Array[#Mutable,T], compareFn: (a: T, b: T) => number): #Array[#Immutable,T];      
      splice         : /\ (start: number): #Array[#Immutable,T]
                       /\ (start: number, deleteCount: number, items: #Array[#Immutable,T]): #Array[#Immutable,T];
      unshift        : (items: #Array[#Immutable,T]): number;
      indexOf        : (searchElement: T, fromIndex: number): number;
      lastIndexOf    : (searchElement: T, fromIndex: number): number;      
      every          : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean): boolean;      
      some           : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean): boolean;      
      forEach        : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => void): void;      
      map            : forall U . (callbackfn: (value: T) => U): #Array[#Immutable, U];
      filter         : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean): #Array[#Immutable,T];
      reduce         : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: #Array[#Immutable,T]) => T, initialValue: T): T;
      reduceRight    : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: #Array[#Immutable,T]) => T, initialValue: T): T;
      length         : { v: number | (v = (len this) && v >= 0) };
    } */

      ////TODO
      //reduce         : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
      //reduce         : forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U;
      //reduceRight    : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
      //reduceRight    : forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U;
      //[x: number]    : T;


/*@ extern Array :: {

      new forall M T . (arrayLength: number) => { v: #Array[M,T] | [ (len v) = arrayLength; not (null v) ] };
      forall M T . (arrayLength: number) => { v: #Array[M,T] | [ (len v) = arrayLength; not (null v) ] };

      isArray         : forall M T . (arg: #Array[M,T]) => { v: boolean | Prop(v) };
      isArray         : forall A . (arg: A) => boolean ;

    } */

      ////TODO
      //prototype: Array<any>;

/*************************************************************************/
/************** Run-Time Tags ********************************************/
/*************************************************************************/

/*@ measure ttag :: forall A . (A) => string                             */

/*@ measure FLS  :: forall A . (A) => bool                               */

/*@ measure Prop :: forall A . (A) => bool                               */

/*@ measure null :: forall A . (A) => bool                               */

/*@ extern builtin_PrefixTypeof :: forall A. (x:A) 
                                => {v:string | (ttag x) = v }            */

/*@ extern builtin_BITruthy :: forall A. (x:A) 
                            => { v:boolean | ((Prop v) <=> Prop(x)) }    */

/*@ extern builtin_BIFalsy :: forall A. (x:A) 
                           => { v:boolean | ((Prop v) <=> FLS(x)) }      */

/*@ invariant {v:undefined | [(ttag(v) = "undefined"); not (Prop(v))]}            */

/*@ invariant {v:null      | [(ttag(v) = "object")   ; not (Prop(v)); null(v) ]}  */

/*@ invariant {v:boolean   | [(ttag(v) = "boolean")]}                             */ 

/*@ invariant {v:string    | [(ttag(v) = "string"   ); (Prop(v) <=> v /= ""  )]}  */

/*@ invariant {v:number    | [(ttag(v)  =  "number");
                              (Prop(v) <=> v /= 0  ); 
                              (FLS(v)  <=> v  = 0  )]} */


/*@ measure instanceof :: forall A . (A, string) => bool                          */

/*@ extern builtin_OpInstanceof :: forall A . (x:A, s: string) 
                                => { v: boolean | (Prop(v) <=> instanceof(x,s)) } */



/*************************************************************************/
/************** Pre-Loaded Qualifiers ************************************/
/*************************************************************************/

/*@ qualif Bot(v:a)                  : 0 = 1                    */
/*@ qualif Bot(v:obj)                : 0 = 1                    */
/*@ qualif Bot(v:boolean)            : 0 = 1                    */
/*@ qualif Bot(v:number)             : 0 = 1                    */
/*@ qualif CmpZ(v:number)            : v < 0                    */
/*@ qualif CmpZ(v:number)            : v <= 0                   */
/*@ qualif CmpZ(v:number)            : v >  0                   */
/*@ qualif CmpO(v:number)            : v >  1                   */
/*@ qualif CmpZ(v:number)            : v >= 0                   */
/*@ qualif CmpZ(v:number)            : v =  0                   */
/*@ qualif CmpZ(v:number)            : v != 0                   */

/*@ qualif Cmp(v:number, x:number)   : v <  x                   */
/*@ qualif Cmp(v:number, x:number)   : v <= x                   */
/*@ qualif Cmp(v:number, x:number)   : v >  x                   */
/*@ qualif Cmp(v:number, x:number)   : v >= x                   */

/*@ qualif Cmp(v:a,x:a)              : v =  x                   */
/*@ qualif Cmp(v:a,x:a)              : v != x                   */
/*@ qualif One(v:number)             : v = 1                    */
/*@ qualif True(v:boolean)           : (? v)                    */
/*@ qualif False(v:boolean)          : not (? v)                */
/*@ qualif True1(v:boolean)          : (Prop v)                 */
/*@ qualif False1(v:boolean)         : not (Prop v)             */


// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*  qualif Add(v:number,x:number,y:number): v = x + y           */
/*  qualif Sub(v:number,x:number,y:number): v = x - y           */

/*  qualif Len(v:number, n: number)  : n < (len v)              */



/*************************************************************************/
/*************  Error handling   *****************************************/
/*************************************************************************/

// NOTE: types that are defined in lib.d.ts need to be in comment to pass
// through the TS compilation phase.

/*@ interface Error<M> {
      name    : string; 
      message : string;
  } */

/*@ extern Error :: {
      new (message: string) => #Error;
      (message: string) => #Error;
      prototype : #Error;
  } */ 
; // XXX: IMPORTANT  -- keep the empty statement here !



//class Errors<M> {

//  /*@ argument :: (argument: string, message: string) => #Error */
//  public static argument(arg: string, message: string): Error {
//    return new Error("Invalid argument: " + arg + ". " + message);
//  }

//  /*@ argumentOutOfRange :: (arg: string) => #Error */
//  public static argumentOutOfRange(arg: string): Error {
//    return new Error("Argument out of range: " + arg);
//  }

//  /*@ argumentNull :: (arg: string) => #Error */
//  public static argumentNull(arg: string): Error {
//    return new Error("Argument null: " + arg);
//  }

//  /*@ abstract :: () => #Error */
//  public static abstract(): Error {
//    return new Error("Operation not implemented properly by subclass.");
//  }

//  /*@ notYetImplemented :: () => #Error */
//  public static notYetImplemented(): Error {
//    return new Error("Not yet implemented.");
//  }

//  /*@ invalidOperation :: (message: string) => #Error */
//  public static invalidOperation(message?: string): Error {
//    return new Error("Invalid operation: " + message);
//  }
//}


/*************************************************************************/
/*******************  Auxilliary *****************************************/
/*************************************************************************/
interface Pair<A,B> { }


/*************************************************************************/
/******************  Mutability  *****************************************/
/*************************************************************************/

interface ReadOnly                        {                      }

interface Immutable      extends ReadOnly { immutable__   : void; } 

interface Mutable        extends ReadOnly { mutable__     : void; } 

interface AnyMutability  extends ReadOnly { defaultMut__  : void; } 

interface InheritedMut   extends ReadOnly { defaultMut__  : void; } 

