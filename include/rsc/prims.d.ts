
 /*  builtin_BIBracketRef :: <M,A>(x: Array<M,A>, n: number + undefined) => A + undefined */
 /*  builtin_BIBracketRef :: <M,A>(x: Array<M,A>, n: undefined) => undefined */
 /*  builtin_BIBracketRef :: <A>  (o: [Immutable] {[y: string]: A }, x: string) => { A | has Property(x,o) } + { undefined | not (hasProperty(x,o)) } */

/*@ builtin_BIBracketRef :: <A>(x: IArray<A> , n: idx<x>)  => A */
/*@ builtin_BIBracketRef :: <A>(x: MArray<A> , n: number)  => A + undefined */
/*@ builtin_BIBracketRef :: <A>({[y: string]: A }, string) => A + undefined */
declare function builtin_BIBracketRef<A>(a: A[], n: number): A;

// TODO : add case for A<AssignsFields> or A<Unique>

/*@ builtin_BIBracketAssign :: <A>(x: IArray<A> , n: idx<x>, v: A) => void */
/*@ builtin_BIBracketAssign :: <M extends ReadOnly,A>(x: Array<M,A>, n: number, v: A) => void */
/*@ builtin_BIBracketAssign :: <A>(a: {[y: string]: A}, s: string, v: A) => void */
declare function builtin_BIBracketAssign<A>(a: any, s: any, v: A): void;

/*@ builtin_BISetProp :: <A,M>({ f?: [M] A }      , A) => { A | true } */    // XXX: UniqueMutable ??
/*@ builtin_BISetProp :: <A>  ({ f?: [Mutable] A }, A) => { A | true } */
declare function builtin_BISetProp<A>(o: { f: A }, v: A): A;

/*@ builtin_BIArrayLit :: <M,A>(A) => {v: Array<M,A> | (len v) = builtin_BINumArgs } */
declare function builtin_BIArrayLit<A>(a: A): A[];

/*@ builtin_BICondExpr :: <C,T>(c: C, t: T, x: T, y: T) => { v: T | (if (Prop(c)) then (v ~~ x) else (v ~~ y)) } */
declare function builtin_BICondExpr<C, T>(c: C, t: T, x: T, y: T): T;

// type CAST_T = any

/*  builtin_BICastExpr :: <V extends CAST_T>(x: V) => { v: V | v = x } */
/*  builtin_BICastExpr :: <V extends CAST_T>(x: V) => V */
// declare function builtin_BICastExpr<V extends CAST_T>(x: V): V;

/*@ builtin_OpLT :: (x:number, y:number) => {v:boolean | Prop v <=> x < y } */
/*@ builtin_OpLT :: <T>(x:T, y:T) => boolean */
declare function builtin_OpLT(a: any, b: any): boolean;

/*@ builtin_OpLEq ::    (x:number, y:number) => {v:boolean | Prop v <=> x <= y } */
/*@ builtin_OpLEq :: <T>(x:T, y:T) => boolean */
declare function builtin_OpLEq(a: any, b: any): boolean;

/*@ builtin_OpGT ::    (x:number, y:number) => {v:boolean | Prop v <=> x > y } */
/*@ builtin_OpGT :: <T>(x:T, y:T) => boolean */
declare function builtin_OpGT(a: any, b: any): boolean;

/*@ builtin_OpGEq :: (x:number, y:number) => {v:boolean | Prop v <=> x >= y } */
/*@ builtin_OpGEq :: <T>(x:T, y:T) => boolean */
declare function builtin_OpGEq(a: any, b: any): boolean;

/*@ builtin_OpAdd :: (x: number, y: number) => {number | v = x + y} */
/*@ builtin_OpAdd :: (x: bitvector32, y: bitvector32) => bitvector32 */
/*@ builtin_OpAdd :: (x: number, y: string) => string */
/*@ builtin_OpAdd :: (x: string, y: number) => string */
/*@ builtin_OpAdd :: (x: string, y: string) => string */
/*@ builtin_OpAdd :: (x: string, y: boolean) => string */
/*@ builtin_OpAdd :: (x: boolean, y: string) => string */
declare function builtin_OpAdd(a: any, b: any): any;

/*@ builtin_OpSub :: (x:number, y:number)  => {v:number | v ~~ x - y} */
declare function builtin_OpSub(a: number, b: number): number;

/*@ builtin_OpMul ::
    (x: number, y: number) => { v:number | [ v = x * y ;
                                            (x > 0 && y > 0) => v > 0 ;
                                            (x < 0 && y < 0) => v > 0 ;
                                            (x = 0 || y = 0) => v = 0 ] }
 */
declare function builtin_OpMul(a: number, b: number): number;

/*@ builtin_OpDiv :: (x: number, {y: number | y != 0}) => {v:number | (x > 0 && y > 1) => (0 <= v && v < x)} */
declare function builtin_OpDiv(a: number, b: number): number;

declare function builtin_OpMod(a: number, b: number): number;

/*@ builtin_PrefixPlus :: (x:number) => {v:number  | v ~~ x} */
declare function builtin_PrefixPlus(a: number): number;

/*@ builtin_PrefixMinus :: (x :number) => {v:number  | v = 0 - x} */
declare function builtin_PrefixMinus(a: number): number;

/*@ builtin_OpSEq :: <A>  (x:A, y:A) => {v:boolean | Prop v <=> x ~~ y } */
/*@ builtin_OpSEq :: <A,B>(x:A, y:B) => {v:boolean | not (Prop v) } */
declare function builtin_OpSEq<A,B>(x: A, y: B): boolean;

/*@ builtin_OpSNEq :: <A>  (x:A, y:A) => {v:boolean | Prop v <=> not (x ~~ y) } */
/*@ builtin_OpSNEq :: <Α,B>(x:A, y:B) => {v:boolean | Prop v } */
declare function builtin_OpSNEq<A,B>(x: A, y: B): boolean;

/*@ builtin_OpLAnd :: <B>  (x: undefined, y:B) => undefined */
/*@ builtin_OpLAnd :: <B>  (x: null, y:B) => null */
/*@ builtin_OpLAnd :: <A>  (x: A, y: A) => { v: A   | if (Prop x) then v = y else v = x } */
/*@ builtin_OpLAnd :: <A,B>(x: A, y: B) => { v: top | Prop v <=> (Prop x && Prop y) } */
declare function builtin_OpLAnd(x: any, y: any): any;
//
// /*@ builtin_OpLOr ::
//     /\ forall A. (x: undefined, y:A) => { v:A | v ~~ y }
//     /\ forall A. (x: null, y:A) => { v:A | v ~~ y }
//     /\ forall A. (x:A, y:A) => { v:A | if (not (Prop x)) then (v = y) else (v = x) }
//     /\ forall A B. (x:A, y:B)  => { v:top | (Prop(v) <=> (Prop(x) || Prop(y))) }
//  */
// declare function builtin_OpLOr(x: any, y: any): any;
//
// /*@ builtin_PrefixLNot ::
//     forall A. (x: A) => {v:boolean | (Prop v) <=> (not (Prop x))}
//  */
// declare function builtin_PrefixLNot<A>(x: A): boolean;
//
// /*@ builtin_PrefixBNot ::
//     (x: number) => {v:number | v = 0 - (x + 1) }
//  */
// declare function builtin_PrefixBNot(n: number): number;
//
// /*@ builtin_OpBOr ::
//     (a: bitvector32, b: bitvector32) => { v: bitvector32 | v = bvor(a,b) }
//  */
// declare function builtin_OpBOr(a: number, b: number): number;
// declare function builtin_OpBXor(a: number, b: number): number;
//
// /*@ builtin_OpBAnd ::
//     (a: bitvector32, b: bitvector32) => { v: bitvector32 | v = bvand(a,b) }
//  */
// declare function builtin_OpBAnd(a: number, b: number): number;
// declare function builtin_OpLShift(a: number, b: number): number;
// /*@ builtin_OpSpRShift ::
//     (a: { number | v >= 0 }, b: { number | v >= 0}) => { number | v >= 0 }
//  */
// declare function builtin_OpSpRShift(a: number, b: number): number;
// declare function builtin_OpZfRShift(a: number, b: number): number;
//
// /*   predicate bv_truthy(b) = (b /= (lit "#x00000000" (BitVec (Size32 obj)))) */


/*************************************************************************
 *
 *          RUN-TIME TAGS
 *
 ************************************************************************/

/*@ builtin_PrefixTypeof :: <A>(x:A) => { v:string | ttag x = v } */
declare function builtin_PrefixTypeof<A>(x: A): string;

/*@ builtin_BITruthy :: (b: bitvector32) => { v: boolean | Prop v <=> b /= lit "#x00000000" (BitVec (Size32 obj)) } */
/*@ builtin_BITruthy :: <A>(x:A)  => { v: boolean | Prop v <=> Prop x } */
declare function builtin_BITruthy<A>(x: A): boolean;

/*@ builtin_BIFalsy  :: <A>(x:A) => { v:boolean | Prop v <=> not (Prop x) } */
declare function builtin_BIFalsy<A>(x: A): boolean;

// // HACK
// /*@ invariant {v: undefined | [(ttag(v) = "undefined"); not (Prop v) ]} */
// /*@ invariant {v: null      | [(ttag(v) = "object"   ); not (Prop v) ]} */
// /*@ invariant {v: boolean   | [(ttag(v) = "boolean"  )]} */
// /*@ invariant {v: string    | [(ttag(v) = "string"   ); (Prop(v) <=> v /= "" )]} */
// /*@ invariant {v: number    | [(ttag(v) = "number"   ); (Prop(v) <=> v /= 0  )]}	*/
//

/*************************************************************************
 *
 *  GENERAL PURPOSE AUXILIARY DEFINITIONS
 *
 ************************************************************************/

declare function crash<A>(): A;

/*@ assume :: <A>(x:A) => {v:void | Prop x} */
declare function assume<A>(x: A): void;

/*@ assert :: <A>({x:A|(Prop x)}) => void */
declare function assert<A>(x: A): void;

declare function random(): number;

declare function pos(): posint;

declare function alert(s: string): void;
//
// /*@ isNaN :: (x:undefined + number) => {v:boolean | Prop v <=> (ttag(v) != "number")} */
// declare function isNaN(x:any) : boolean;
