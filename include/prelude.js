/*************************************************************************/
/*********** General purpose auxiliary functions *************************/
/*************************************************************************/
/*@ type #List[A] { data: A, next: #List[A] } */


/*@ measure len      :: forall A. (#List [A])          => number                                              */
/*@ extern cons      :: forall A. (A, xs:#List[A] + null)
=> {#List[A] | (len v) = 1 + (len xs)}                 */
/*@ extern nil       ::           ()                   => { null | (len v) = 0}                               */
/*@ extern head      :: forall A. (xs:#List[A])        => A                                                   */
/*@ extern tail      :: forall A. (xs:#List [A])       => #List[A] + null                                     */
/*@ extern nth       :: forall A. (xs:#List [A], {i:number| ((0 <= i) && i < (len xs))})
=> A                                                   */
/*@ extern empty     :: forall A. (x: #List[A] + null) =>
{v: boolean | (((Prop v) <=> len(x) = 0) && ((Prop v) <=> ttag(x) = "null"))}         */
/*@ extern emptyPoly :: forall A. (x:A)                => {v: boolean | ((Prop v) <=> ((ttag x) = "null"))}   */
/*@ extern mylength  :: forall A. (xs:#List[A] + null) => {v:number | ((v >= 0) && v = (len xs))}             */
/*@ extern safehead  :: forall A. (#List[A])           => A                                                   */
/*@ extern safetail  :: forall A. (xs:#List[A])        => {v:#List[A] + null | (len v) = (len xs) - 1}        */
/* extern Array      :: (n : { v: number | 0 <= v } ) => { v: [ undefined ] | (len v) = n }                   */
/*************************************************************************/
/************************* Type Conversions ******************************/
/*************************************************************************/
/*@ extern sstring  :: forall A. (x: A) => string                               */
/*************************************************************************/
/************** Types for Builtin Operators ******************************/
/*************************************************************************/
/*@ extern builtin_BIBracketRef     :: forall A. (arr:[A], {idx:number | (0 <= idx && idx < (len arr))}) => A           */
/*@ extern builtin_BIBracketAssign  :: forall A. (arr:[A], {idx:number | (0 <= idx && idx < (len arr))}, val:A) => void */
/*@ extern builtin_BIArrayLit       :: forall A. (A) => {v:[A] | (len v) = builtin_BINumArgs}                           */
/*@ extern builtin_BIUndefined      :: forall A. {A | false}                                                            */
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
/*@ extern builtin_OpSub       :: ({x:number | true}, {y:number | true})  => {v:number | v = x - y} */
/*@ extern builtin_OpMul       :: (number,  number)  => number                                      */
/*@ extern builtin_OpDiv       :: (number,  number)  => number                                      */
/*@ extern builtin_OpMod       :: (number,  number)  => number                                      */
/*@ extern builtin_PrefixMinus :: ({x:number  | true}) => {v:number  | v = (0 - x)}                 */
//PV: @==@ and @===@ could be handled more precisely
/*@ extern builtin_OpEq        :: forall A.   (x:A, y:A) => {v:boolean | ((Prop v) <=> (x = y)) }   */
/*@ extern builtin_OpSEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x = y)) }   */
/*@ extern builtin_OpNEq       :: forall A B. (x:A, y:B) => {v:boolean | ((Prop v) <=> (x != y)) }  */
/*@ extern builtin_OpLAnd      :: (x:top, y:top)         => {v:top     | ((Prop v) <=> (if (TRU(x)) then (v = y) else (v = x) ))} */
/*@ extern builtin_OpLOr       :: (x:top, y:top)         => {v:top     | ((Prop v) <=> (if (FLS(x)) then (v = y) else (v = x) ))} */
/*  OLDER VERSION: */
/*  extern builtin_OpLOr       :: (x:boolean, y:boolean) => {v:boolean | ((Prop v) <=> ((Prop x) || (Prop y)))}                  */
/*@ extern builtin_PrefixLNot  :: (x:boolean)            => {v:boolean | ((Prop v) <=> not (Prop x))}                            */
//XXX: Is there an issue with keeping this with a capital P???
/*@ measure Prop        :: (boolean) => boolean                              */
/*************************************************************************/
/************* Ambient Definitions ***************************************/
/*************************************************************************/
/*************************************************************************/
/** Taken from here: *****************************************************/ /** http://typescript.codeplex.com/SourceControl/latest#typings/lib.d.ts */
/*************************************************************************/
/*** Number **************************************************************/
/*@ extern NumberC :: (x: top) => number */
/*** Math ****************************************************************/
/*@ extern Math :: {
E       : number,
LN10    : number,
LN2     : number,
LOG2E   : number,
LOG10E  : number,
PI      : number,
SQRT1_2 : number,
SQRT2   : number,
abs     : (x: number) => number,
acos    : (x: number) => number,
asin    : (x: number) => number,
atan    : (x: number) => number,
atan2   : (y: number, x: number) =>  number,
ceil    : (x: number) => number,
cos     : (x: number) => number,
exp     : (x: number) => number,
floor   : (x: number) => number,
log     : (x: number) => number,
max     : (values: [number]) => number,
min     : (values: [number]) => number,
pow     : (x: number, y: number) => number,
random  : () =>  number,
round   : (x: number) => number,
sin     : (x: number) => number,
sqrt    : (x: number) => number,
tan     : (x: number) => number
} */
/*** String **************************************************************/
/*@ extern String     :: {
toString          : () => string,
charAt            : (pos: number) => string,
charCodeAt        : (index: number) => number,
concat            : (strings: [string]) => string,
indexOf           : (searchString: string, position: number) => number,
lastIndexOf       : (searchString: string, position: number) => number,
localeCompare     : (that: string) => number,
match             : (regexp: string) => [string],
replace           : (searchValue: string, replaceValue: string) => string,
search            : (regexp: string) => number,
slice             : (start: number, end: number) => string,
split             : (separator: string, limit: number) => [string],
substring         : (start: number, end: number) => string,
toLowerCase       : () => string,
toLocaleLowerCase : () => string,
toUpperCase       : () => string,
toLocaleUpperCase : () => string,
trim              : () => string,

length            : number,

substr            : (from: number, length: number) => string
} */
// Typescript Definition:
// extern String     : {
//     toString          : () => string,
//     charAt            : (pos: number) => string,
//     charCodeAt        : (index: number) => number,
//     concat            : (...strings: [string]) => string,
//     indexOf           : (searchString: string, position?: number) => number,
//     lastIndexOf       : (searchString: string, position?: number) => number,
//     localeCompare     : (that: string) => number,
//     match             : (regexp: string) => [string],
//     match             : (regexp: RegExp) => [string],
//     replace           : (searchValue: string, replaceValue: string) => string,
//     replace           : (searchValue: string, replaceValue: (substring: string, ...args: [top]) => string) => string,
//     replace           : (searchValue: RegExp, replaceValue: string) => string,
//     replace           : (searchValue: RegExp, replaceValue: (substring: string, ...args: [top]) => string) => string,
//     search            : (regexp: string) => number,
//     search            : (regexp: RegExp) => number,
//     slice             : (start: number, end?: number) => string,
//     split             : (separator: string, limit?: number) => [string],
//     split             : (separator: RegExp, limit?: number) => [string],
//     substring         : (start: number, end?: number) => string,
//     toLowerCase       : () => string,
//     toLocaleLowerCase : () => string,
//     toUpperCase       : () => string,
//     toLocaleUpperCase : () => string,
//     trim              : () => string,
//
//     length: number,
//
//     substr            : (from: number, length?: number) => string
// }
/*** Number **************************************************************/
/*@ extern Number :: {
toString        :  /\ (radix: number) => string
/\ () => string,
toFixed         : (fractionDigits: number) => string,
toExponential   : (fractionDigits: number) => string,
toPrecision     : (precision: number) => string
} */
// Typescript Definition:
// extern Number : {
//     toString        : (radix?: number) => string
//     toFixed         : (fractionDigits?: number) => string,
//     toExponential   : (fractionDigits?: number) => string,
//     toPrecision     : (precision: number) => string
// }
/*** Array ***************************************************************/
/*@ interface Array<T> {
toString       : () => string,
toLocaleString : () => string,
concat         : (items: [T]) => [T],
join           : (separator: string) => string,
pop            : () => T,
push           : (items: [T]) => number,
reverse        : () => [T],
shift          : () => T,
slice          : (start: number, end: number) => [T],
sort           : (compareFn: (a: T, b: T) => number) => [T],
splice         :  /\ (start: number) => [T]
/\ (start: number, deleteCount: number, items: [T]) => [T],
unshift        : (items: [T]) => number,

indexOf        : (searchElement: T, fromIndex: number) => number,
lastIndexOf    : (searchElement: T, fromIndex: number) => number,
every          : (callbackfn: (value: T, index: number, array: [T]) => boolean) => boolean,
some           : (callbackfn: (value: T, index: number, array: [T]) => boolean) => boolean,
forEach        : (callbackfn: (value: T, index: number, array: [T]) => void) => void,
map            : forall U . (callbackfn: (value: T) => U) => [U],
filter         : (callbackfn: (value: T, index: number, array: [T]) => boolean) => [T],

reduce         : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T,

reduceRight    : (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T,

length         : { v: number | v = (len this) }
}
*/
/*
//TODO
reduce         :  /\ (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
/\ forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U,

reduceRight    :  /\ (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue: T) => T
/\ forall U . (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U,
*/
// Typescript Definition:
//  interface Array[T] =
//   {
//     toString              :: () => string,
//     toLocaleString        :: () => string,
//     concat<U extends T[]> :: (...items: [U]) => [T],
//     concat                :: (...items: [T]) => [T],
//     join                  :: (separator?: string) => string,
//     pop                   :: () => T,
//     push                  :: (...items: [T]) => number,
//     reverse               :: () => [T],
//     shift                 :: () => T,
//     slice                 :: (start: number, end?: number) => [T],
//     sort                  :: (compareFn?: (a: T, b: T) => number) => [T],
//     splice                :: (start: number) => [T],
//     splice                :: (start: number, deleteCount: number, ...items: [T]) => [T],
//     unshift               :: (...items: [T]) => number,
//
//     indexOf               :: (searchElement: T, fromIndex?: number) => number,
//     lastIndexOf           :: (searchElement: T, fromIndex?: number) => number,
//     every                 :: (callbackfn: (value: T, index: number, array: [T]) => boolean, thisArg?: any) => boolean,
//     some                  :: (callbackfn: (value: T, index: number, array: [T]) => boolean, thisArg?: any) => boolean,
//     forEach               :: (callbackfn: (value: T, index: number, array: [T]) => void, thisArg?: any) => void,
//     map<U>                :: (callbackfn: (value: T, index: number, array: [T]) => U, thisArg?: any) => [U],
//     filter                :: (callbackfn: (value: T, index: number, array: [T]) => boolean, thisArg?: any) => [T],
//     reduce                :: (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue?: T) => T,
//     reduce<U>             :: (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U,
//     reduceRight           :: (callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: [T]) => T, initialValue?: T) => T,
//     reduceRight<U>        :: (callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: [T]) => U, initialValue: U) => U,
//
//     length: number,
//
//     [n: number]: T,
//   }
/*************************************************************************/
/************** Run-Time Tags ********************************************/
/*************************************************************************/
/*@ measure ttag :: forall A. (A) => string                                   */
/*@ measure TRU :: forall A . (A) => Prop                                     */
/*@ extern builtin_PrefixTypeof :: forall A. (x:A)
=> {v:string | (ttag x) = v }             */
/*@ extern builtin_BITruthy :: forall A. (x:A)
=> { v:boolean | ((Prop v) <=> TRU(x)) }    */
/*@ invariant {v:undefined | [(ttag(v) = "undefined"); (TRU(v) <=> false  )]} */
/*@ invariant {v:null      | [(ttag(v) = "null"     ); (TRU(v) <=> false  )]} */
/*@ invariant {v:boolean   | [(ttag(v) = "boolean"  ); (TRU(v) <=> Prop(v))]} */ /*@ invariant {v:number    | [(ttag(v) = "number"   ); (TRU(v) <=> v /= 0 )]} */
/*@ invariant {v:string    | [(ttag(v) = "string"   ); (TRU(v) <=> v /= "")]} */
/*@ invariant {v:{}        | ttag(v) = "object"   }                           */
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
/*@ qualif True(v:Bool)              : (? v)                    */
/*@ qualif False(v:Bool)             : not (? v)                */
/*@ qualif True1(v:Bool)             : (Prop v)                 */
/*@ qualif False1(v:Bool)            : not (Prop v)             */
// Somewhat more controversial qualifiers (i.e. "expensive"...)
/*  qualif Add(v:number,x:number,y:number): v = x + y           */
/*  qualif Sub(v:number,x:number,y:number): v = x - y           */
var __dummy__ = null;
