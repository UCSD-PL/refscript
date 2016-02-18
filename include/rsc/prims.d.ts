
/*@ builtin_BIBracketRef :: <A>(x: UArray<A> , n: idx<x>)  => A */
/*@ builtin_BIBracketRef :: <A>(x: IArray<A> , n: idx<x>)  => A */
/*@ builtin_BIBracketRef :: <A>(x: MArray<A> , n: number)  => A + undefined */
/*@ builtin_BIBracketRef :: <A>(x: {[y: string]: A }, s: string)
    => { A + undefined | (hasProperty x s) => (ttag v != "undefined") } */
declare function builtin_BIBracketRef(a: any, n: any): any;

// TODO : add case for A<AssignsFields> or A<Unique>

/*@ builtin_BIBracketAssign :: <A>(x: UArray<A> , n: idx<x>, v: A) => void */
/*@ builtin_BIBracketAssign :: <M extends ReadOnly, A, B extends A>(x: Array<M,A>, n: number, v: A) => void */
/*@ builtin_BIBracketAssign :: <A>(a: {[y: string]: A}, s: string, v: A) => void */
declare function builtin_BIBracketAssign<A>(a: any, s: any, v: A): void;

/*@ builtin_BIImmArrayLit :: <A>(x: A) => {v: IArray<A> | len v = builtin_BINumArgs } */
declare function builtin_BIImmArrayLit<A>(a: A): A[];

declare function builtin_BIUniqueArrayLit<A>(a: A): Array<Unique, A>;

declare function builtin_BIArrayLit<M extends ReadOnly, A>(a: A): Array<M, A>;

/*@ builtin_BICondExpr :: <C, A, B>(c: C, x: A, y: B) => { v: any | if Prop(c) then v ~~ x else v ~~ y } */
declare function builtin_BICondExpr<C, A, B>(c: C, x: A, y: B): any;

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

/*@ builtin_OpAdd :: (x: number     , y: number     ) => {number | v = x + y} */
/*@ builtin_OpAdd :: (x: real       , y: real       ) => {real   | v = x + y} */
/*@ builtin_OpAdd :: (x: bitvector32, y: bitvector32) => bitvector32          */
/*@ builtin_OpAdd :: (x: number     , y: string     ) => string               */
/*@ builtin_OpAdd :: (x: string     , y: number     ) => string               */
/*@ builtin_OpAdd :: (x: string     , y: string     ) => string               */
/*@ builtin_OpAdd :: (x: string     , y: boolean    ) => string               */
/*@ builtin_OpAdd :: (x: boolean    , y: string     ) => string               */
declare function builtin_OpAdd(a: any, b: any): any;

/*@ builtin_OpSub :: (x:number, y:number)  => {v:number | v ~~ x - y} */
declare function builtin_OpSub(a: number, b: number): number;

/*@ builtin_OpMul ::
    (x: real, y: real) => { v: real | v = x * y } */

/*@ builtin_OpMul ::
    (x: number, y: number) => { v:number | [ v = x * y ;
                                            (x > 0 && y > 0) => v > 0 ;
                                            (x < 0 && y < 0) => v > 0 ;
                                            (x = 0 || y = 0) => v = 0 ] }
 */
declare function builtin_OpMul(a: number, b: number): number;

/*@ builtin_OpDiv :: (x: number, y: { number | v != 0}) => {v:number | (x > 0 && y > 1) => (0 <= v && v < x)} */
declare function builtin_OpDiv(a: number, b: number): number;

declare function builtin_OpMod(a: number, b: number): number;

/*@ builtin_PrefixPlus :: (x:number) => { v:number  | v ~~ x } */
declare function builtin_PrefixPlus(a: number): number;

/*@ builtin_PrefixMinus :: (x :number) => {v:number  | v = 0 - x} */
declare function builtin_PrefixMinus(a: number): number;

/*@ builtin_OpSEq :: <A>  (x:A, y:A) => {v:boolean | Prop v <=> x ~~ y } */
/*@ builtin_OpSEq :: <A,B>(x:A, y:B) => {v:boolean | not (Prop v) } */
declare function builtin_OpSEq<A,B>(x: A, y: B): boolean;

/*@ builtin_OpSNEq :: <A>  (x:A, y:A) => {v:boolean | Prop v <=> not (x ~~ y) } */
/*@ builtin_OpSNEq :: <A,B>(x:A, y:B) => {v:boolean | Prop v } */
declare function builtin_OpSNEq<A,B>(x: A, y: B): boolean;

/*@ builtin_PrefixLNot :: <A>(x: A) => {v:boolean | Prop v <=> not (Prop x) } */
declare function builtin_PrefixLNot<A>(x: A): boolean;

/*@ builtin_OpLAnd ::<B>(x: undefined, y:B) => undefined */
/*@ builtin_OpLAnd ::<B>(x: null, y:B) => null */
/*@ builtin_OpLAnd ::<A>(x:A, y:A) => { v:A | if (Prop x) then (v = y) else (v = x) } */
/*@ builtin_OpLAnd ::<A,B>(x:A, y:B) => { v:top | Prop v <=> (Prop x && Prop y) } */
declare function builtin_OpLAnd(x: any, y: any): any;

// /*@ builtin_PrefixBNot ::
//     (x: number) => {v:number | v = 0 - (x + 1) }
//  */
// declare function builtin_PrefixBNot(n: number): number;

/*@ builtin_OpBOr :: (x: bitvector32, x: bitvector32) => { v: bitvector32 | v = bvor x y } */
/*@ builtin_OpBOr :: <A>(x: A, b: A) => { v: A | if (Prop x) then (v = x) else (v = y) } */
declare function builtin_OpBOr(x: number, y: number): number;

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

declare function builtin_BICtorExit(): void;

// RUN-TIME TAGS

/*@ builtin_PrefixTypeof :: <A>(x:A) => { v:string | ttag x = v } */
declare function builtin_PrefixTypeof<A>(x: A): string;

/*@ builtin_BITruthy :: (b: bitvector32) => { v: boolean | Prop v <=> b /= lit "#x00000000" (BitVec (Size32 obj)) } */
/*@ builtin_BITruthy :: <A>(x:A)  => { v: boolean | Prop v <=> Prop x } */
declare function builtin_BITruthy<A>(x: A): boolean;

/*@ builtin_BIFalsy  :: <A>(x:A) => { v:boolean | Prop v <=> not (Prop x) } */
declare function builtin_BIFalsy<A>(x: A): boolean;


/*@ builtin_BIForInKeys :: <A>(a: IArray<A>) => IArray<idx<a>> */
/*@ builtin_BIForInKeys ::    (o: { }      ) => IArray<{ string | (hasProperty o v) && (enumProp o v) }> */
declare function builtin_BIForInKeys(obj: Object): string[];


// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...in
//TODO: remove the last overload once {[s:string]:A} extends { } PV: ???

/*@ builtin_BIForInKeys :: <A>(a: IArray<A>)         => IArray<{ number | 0 <= v && v < len a }> */
/*@ builtin_BIForInKeys ::    (o: Object<Immutable>) => IArray<{ string | hasProperty o v && enumProp v o }> */
/*@ builtin_BIForInKeys ::    (o: (Immutable) { })   => IArray<{ string | hasProperty o v && enumProp v o }> */
/*@ builtin_BIForInKeys :: <A>(o: (Immutable) { [s:string]:A }) => IArray<{ string | hasProperty o v && enumProp v o }> */
declare function builtin_BIForInKeys(obj: Object): string[];

/*@ builtin_OpInstanceof :: <A>(x:A, s: string) => { v: boolean | Prop v <=> extends_class(x, s) } */
declare function builtin_OpInstanceof<A>(x: A, s: string): boolean;

/*@ builtin_OpIn :: <A>(i: number, a: IArray<A>) => { v: boolean | Prop v <=> (0 <= i && i < len a) } */
/*@ builtin_OpIn ::    (s: string, o: { }      ) => { v: boolean | Prop v <=> hasProperty o s }        */
declare function builtin_OpIn(s: string, obj: Object): boolean;


// INVARIANTS

/*@ invariant {v: undefined | [(ttag(v) = "undefined"); not (Prop v) ]} */
/*@ invariant {v: null      | [(ttag(v) = "object"   ); not (Prop v) ]} */
/*@ invariant {v: boolean   | [(ttag(v) = "boolean"  )]} */
/*@ invariant {v: string    | [(ttag(v) = "string"   ); (Prop(v) <=> v /= "" )]} */
/*@ invariant {v: number    | [(ttag(v) = "number"   ); (Prop(v) <=> v /= 0  )]}	*/

// GENERAL PURPOSE AUXILIARY DEFINITIONS

/*@ crash :: <A>() => A */
declare function crash(): any;

/*@ assume :: <A>(x: A) => {v:void | Prop x} */
declare function assume<A>(x: A): void;

/*@ assert :: <A>({A | Prop v}) => void */
declare function assert<A>(x: A): void;

declare function random(): number;

/*@ pos :: () => posint */
declare function pos(): posint;

declare function alert(s: string): void;

/*@ isNaN :: (x: undefined + number) => {v:boolean | Prop v <=> (ttag v != "number")} */
declare function isNaN(x:any) : boolean;
