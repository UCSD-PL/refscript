
/*@ type LList<M,T> = List<M,T> + null */
declare type LList<M,T> = any;

/*@ measure LLlen :: <M extends ReadOnly, T> (LList<M,T>) => number */

interface List<M extends ReadOnly, T> {
  data: T;
  /*@ next : LList<M, T> */
  next: List<M, T>;
}
/*@ empty :: <M extends ReadOnly, T> (a: LList<M,T>) => {boolean | ((Prop v) <=> not (Prop a)) && ((Prop v) <=> LLlen(a) = 0) } */
declare function empty<M extends ReadOnly, T>(a? : List<M,T>) : boolean;
declare function emptyPoly<M extends ReadOnly, T>(xs: List<M,T>):boolean;
declare function head<M extends ReadOnly, T>(a : List<M,T>) : T;
/*@ tail :: <M extends ReadOnly, T> (a: List<M,T>) => {LList<M,T> | LLlen(v) = LLlen(a) - 1} */
declare function tail<M extends ReadOnly, T>(a : List<M,T>) : List<M,T>;
/*@ nil :: () => {null | LLlen(v) = 0} */
declare function nil() : any;
/*@ cons :: <M extends ReadOnly, T> (hd:T, tl:LList<M,T>) => {LList<M,T> | LLlen(v) = LLlen(tl) + 1} */
declare function cons<M extends ReadOnly, T>(hd:T,tl?:List<M,T>):List<M,T>;
declare function safehead<M extends ReadOnly, T>(a : List<M,T>) : T;
/*@ safetail :: <M extends ReadOnly, T> (a: List<M,T>) => {LList<M,T> | LLlen(v) = LLlen(a) - 1} */
declare function safetail<M extends ReadOnly, T>(a : List<M,T>) : List<M,T>;
/*@ mylength :: <M extends ReadOnly, T> (a: LList<M,T>) => {number | v = LLlen(a)} */
declare function mylength<M extends ReadOnly, T>(a : List<M,T>) : number;
