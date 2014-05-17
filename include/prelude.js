/*************************************************************************/
/*********** General purpose auxiliary functions *************************/
/*************************************************************************/

/*@ measure len      :: forall A. (#List [A])          => number                                            */
/*@ extern cons      :: forall A. (A, xs:#List[A]?)
=> {#List[A] | (len v) = 1 + (len xs)}               */
/*@ extern nil       ::           ()                   => { null | (len v) = 0}                             */
/*@ extern head      :: forall A. (xs:#List[A])        => A                                                 */
/*@ extern tail      :: forall A. (xs:#List [A])       => #List[A]?                                         */
/*@ extern nth       :: forall A. (xs:#List [A], {i:number| ((0 <= i) && i < (len xs))})
=> A                                                 */
/*@ extern empty     :: forall A. (x: #List[A]?) =>
{v: boolean | (((Prop v) <=> len(x) = 0) && ((Prop v) <=> ttag(x) = "null"))}       */
/*@ extern emptyPoly :: forall A. (x:A)                => {v: boolean | ((Prop v) <=> ((ttag x) = "null"))} */
/*@ extern mylength  :: forall A. (xs:#List[A]?) => {v:number | ((v >= 0) && v = (len xs))}                 */
/*@ extern safehead  :: forall A. (#List[A])           => A                                                 */
/*@ extern safetail  :: forall A. (xs:#List[A])        => {v:#List[A]? | (len v) = (len xs) - 1}            */
/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/
/*@ extern builtin_BIBracketRef     ::
/\ forall A. (arr: #Array[#Immutable,A], {idx:number | (0 <= idx && idx < (len arr))}) => A
/\ forall A. (arr: #Array[#Mutable, A],  idx:number                                 ) => A?
/\ forall A. ({[y: string]: A }       ,    x:string                                  ) => A      */
/*@ extern builtin_BIBracketAssign  ::
/\ forall A. (arr: #Array[#Immutable, A], {idx:number | (0 <= idx && idx < (len arr))}, val: A) => void
/\ forall A. (arr: #Array[#ReadOnly , A],  idx:number                                 , val: A) => void
/\ forall A. ({[y: string]: A },            x:string                                  , val: A) => void
*/
//FIXME: the 'len' property is invalid if M != Immutable
/*@ extern builtin_BIArrayLit  :: forall M  A . (A)
=> {v: #Array[M,A] | (len v) = builtin_BINumArgs}                    */
/*@ extern builtin_BIUndefined :: forall A. {A | false}                                             */
/*@ extern builtin_OpLT        :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <  y)) }
/\ (x:string, y:number) => boolean
/\ (x:number, y:number) => boolean
/\ (x:string, y:string) => boolean                                */
/*@ extern builtin_OpLEq       :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x <= y)) }
/\ (x:string, y:number) => boolean
/\ (x:number, y:number) => boolean
/\ (x:string, y:string) => boolean                                */
/*@ extern builtin_OpGT        :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >  y)) }
/\ (x:string, y:number) => boolean
/\ (x:number, y:number) => boolean
/\ (x:string, y:string) => boolean                                */
/*@ extern builtin_OpGEq       :: /\ (x:number, y:number) => {v:boolean | ((Prop v) <=> (x >= y)) }
/\ (x:string, y:number) => boolean
/\ (x:number, y:number) => boolean
/\ (x:string, y:string) => boolean                                */
/*@ extern builtin_OpAdd       :: /\ (x:number, y:number) => {number | v = x + y}
/\ (x:number, y:string) => string
/\ (x:string, y:number) => string
/\ (x:string, y:string) => string                                 */
/*@ extern builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v ~~ x - y} */
/*@ extern builtin_OpMul       :: (number,  number)  => number                                      */
//FIXME: This is not correct. Add definition for: >>
/*@ extern builtin_OpDiv       :: (x: number, y: { v: number | v != 0 })
=> { v:number | (    ((x>0 && y>0) => v>0)
&& (x=0 <=> v=0)
&& ((x>0 && y>1) => v<x)
)}
*/
/*@ extern builtin_OpMod       :: (number,  number)  => number                                      */
/*@ extern builtin_PrefixMinus :: ({x:number  | true}) => {v:number  | v ~~ (0 - x)}                 */
/*@ extern builtin_OpEq        :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x = y)) }   */
/*@ extern builtin_OpSEq       :: /\ forall A  . (x:A,    y: null) => {v:boolean | ((Prop v) <=> (ttag(x) = "null")) }
/\ forall A  . (x:null, y:A)     => {v:boolean | ((Prop v) <=> (ttag(y) = "null")) }
/\ forall A  . (x:A,    y:A)     => {v:boolean | ((Prop v) <=> (x = y)) }
/\ forall A B. (x:A,    y:B)     => {v:boolean | ((Prop v) <=> ((ttag(x) = ttag(y)) && (x = y))) }  */
/*@ extern builtin_OpNEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) }  */
/*@ extern builtin_OpSNEq      :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) }  */
// FIXME: the two version of inequality should not be the same...
/*@ extern builtin_OpLAnd      :: (x:top, y:top)         => {v:top | ((Prop v) <=> (if (TRU(x)) then (v = y) else (v = x) ))}     */
/*@ extern builtin_OpLOr       :: forall A . (x:A, y:A)  => {v:A   | ((Prop v) <=> (if (FLS(x)) then (v = y) else (v = x) ))}     */
/*@ extern builtin_PrefixLNot  :: forall A . (x: A)      => {v:boolean | (((Prop v) <=> not TRU(x)) && ((Prop v) <=> FLS(x)))}     */
/*@ extern builtin_PrefixBNot  :: (x: number)            => {v:number | v = 0 - (x + 1) }           */
/*************************************************************************
Ambient Definitions
Taken from here:
http://typescript.codeplex.com/sourcecontrol/latest#typings/core.d.ts
**************************************************************************/
/*** Object **************************************************************/
/*@ interface Object<M> {
toString            : () => string;
toLocaleString      : () => string;
valueOf             : () => #Object;
hasOwnProperty      : (v: string) => boolean;
isPrototypeOf       : forall A . (v: A) => boolean;
propertyIsEnumerable: (v: string) => boolean;
} */
/*  TODO:
constructor: Function;
*/
/*@ extern Object :: {
new forall A . (value: A) => #Object;
forall A .     (value: A) => top;
prototype           : #Object;
getPrototypeOf      : forall A . (o: A) => top;
getOwnPropertyNames : forall A . (o: A) => [string];
keys                : forall A . (o: A) => [string];
} */
/*  TODO:
(): any;
getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor;
create(o: any, properties?: PropertyDescriptorMap): any;
defineProperty(o: any, p: string, attributes: PropertyDescriptor): any;
defineProperties(o: any, properties: PropertyDescriptorMap): any;
seal(o: any): any;
freeze(o: any): any;
preventExtensions(o: any): any;
isSealed(o: any): boolean;
isFrozen(o: any): boolean;
isExtensible(o: any): boolean;
*/
/*** Number **************************************************************/
/*@ extern NumberC :: (x: top) => number */
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
max     : (values: [number]) => number;
min     : (values: [number]) => number;
pow     : (x: number, y: number) => number;
random  : () =>  number;
round   : (x: number) => number;
sin     : (x: number) => number;
sqrt    : (x: number) => number;
tan     : (x: number) => number
} */
/*** String **************************************************************/
/*@ extern String     :: {
toString          : () => string;
charAt            : (pos: number) => string;
charCodeAt        : (index: number) => number;
concat            : (strings: [string]) => string;
indexOf           : (searchString: string, position: number) => number;
lastIndexOf       : (searchString: string, position: number) => number;
localeCompare     : (that: string) => number;
match             : (regexp: string) => [string];
replace           : (searchValue: string, replaceValue: string) => string;
search            : (regexp: string) => number;
slice             : (start: number, end: number) => string;
split             : (separator: string, limit: number) => [string];
substring         : (start: number, end: number) => string;
toLowerCase       : () => string;
toLocaleLowerCase : () => string;
toUpperCase       : () => string;
toLocaleUpperCase : () => string;
trim              : () => string;
length            : { number | v >= 0 };
substr            : (from: number, length: number) => string
} */
/*** Number **************************************************************/
/*@ extern Number :: {
toString        :  /\ (radix: number) => string
/\ () => string;
toFixed         : (fractionDigits: number) => string;
toExponential   : (fractionDigits: number) => string;
toPrecision     : (precision: number) => string
} */
/*** Array ***************************************************************/
/*@ interface Array<M,T> {
toString       : () => string;
toLocaleString : () => string;
concat [#Array[#Immutable,T]] : /\ (items: #Array[#Immutable,T]) => { #Array[#Immutable,T] | (len v) = (len this) + (len items) }
/\ forall M1 M2 . (items: #Array[M1,T]) => #Array[M2,T];
concat [#Array[#ReadOnly ,T]] :    forall M1 M2 . (items: #Array[M1,T]) => #Array[M2,T];
join           : (separator: string) => string;
pop    [#Array[#Mutable,  T]] : () => T;
push   [#Array[#Mutable,  T]] : (items: T) => number;
reverse[#Array[M,T]]          : () => #Array[M,T];
shift          : () => T;
slice          : /\ forall N . (start: number)              => #Array[N,T]
/\ forall N . (start: number, end: number) => #Array[N,T];
sort   [#Array[#Mutable,  T]] : (compareFn: (a: T, b: T) => number) => #Array[#Immutable,T];
splice         :  /\ (start: number) => #Array[#Immutable,T]
/\ (start: number, deleteCount: number, items: #Array[#Immutable,T]) => #Array[#Immutable,T];
unshift        : (items: #Array[#Immutable,T]) => number;
indexOf        : (searchElement: T, fromIndex: number) => number;
lastIndexOf    : (searchElement: T, fromIndex: number) => number;
every          : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean) => boolean;
some           : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean) => boolean;
forEach        : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => void) => void;
map            : forall U . (callbackfn: (value: T) => U) => [U];
filter         : (callbackfn: (value: T, index: number, array: #Array[#Immutable,T]) => boolean) => #Array[#Immutable,T];
reduce         : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: #Array[#Immutable,T]) => T, initialValue: T) => T;
reduceRight    : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: #Array[#Immutable,T]) => T, initialValue: T) => T;
length [#Array[#Immutable,T]] : { v: number | (v = (len this) && v >= 0) };
length         : { v: number | (v >= 0) };
}
*/
/*
//TODO
reduce         :  /\ (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
/\ forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U;
reduceRight    :  /\ (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
/\ forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U;
[x: number]    : T;
*/
/*@ extern Array :: {
new forall M T . (arrayLength: number) => { v: #Array[M,T] | (len v) = arrayLength } ;
forall M T     . (arrayLength: number) => { v: #Array[M,T] | (len v) = arrayLength } ;
isArray  : /\ forall M T . (arg: #Array[M,T]) => { v: boolean | Prop(v) }
/\ forall A   . (arg: A)           => boolean ;
} */
/*
//TODO
prototype: Array<any>;
*/
/*************************************************************************/
/************** Run-Time Tags ********************************************/
/*************************************************************************/
/*@ measure ttag :: forall A. (A) => string                                   */
/*@ measure TRU  :: forall A . (A) => bool                                    */
/*@ measure FLS  :: forall A . (A) => bool                                    */
/*@ measure Prop :: forall A . (A) => bool                                    */
/*@ extern builtin_PrefixTypeof :: forall A. (x:A)
=> {v:string | (ttag x) = v }             */
/*@ extern builtin_BITruthy :: forall A. (x:A)
=> { v:boolean | ((Prop v) <=> TRU(x)) }    */
/*@ extern builtin_BIFalsy :: forall A. (x:A)
=> { v:boolean | ((Prop v) <=> FLS(x)) }    */
/*@ invariant           {v:undefined | [not (TRU(v))        ]} */
/*@ invariant           {v:null      | [not (TRU(v))        ]} */
/*@ invariant           {v:boolean   | [(TRU(v) <=> Prop(v))]} */ /*@ invariant           {v:number    | [(TRU(v) <=> v /= 0 )]} */
/*@ invariant           {v:string    | [(TRU(v) <=> v /= "")]} */
/*  invariant forall A. {v:[A]       |                       } */
/*  invariant           {v:{}        |                       } */
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
name: string;
message: string;
} */
/*@ extern Error :: {
new (message: string) => #Error;
(message: string) => #Error;
prototype: #Error;
} */
var __ddd = 1;


var __dummy__ = null;
