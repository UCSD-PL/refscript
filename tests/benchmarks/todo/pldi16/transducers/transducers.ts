// Copyright 2014 Cognitect. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.


//TODO: restore "ATATiterator" to "@@iterator"

/*@ qualif HasP (x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP(x: string, y: A): enumProp(x, y) */


module com {
module cognitect {
module transducers {

/*@ predicate Inst(X, Key, Type) = (hasProperty(Key, X) <=> extends_interface(X, Type)) */

/*@ predicate InstIterator(V) = Inst(V,"next","Iterator") */
/*@ predicate InstIterable(V) = Inst(V,"ATATiterator","Iterable") */

/*@ predicate IsIter(V) = extends_interface(V,"Iterator") || extends_interface(V,"Iterable") */

/*@ type ObjectK = { v: EmptyObject<Immutable> | InstIterator(v) && InstIterable(v) } */
/*@ type ObjIter   = { v: ObjectK |     IsIter(v) } */
/*@ type ObjNoIter = { v: ObjectK | not IsIter(v) } */

/*@ type MTransformer<T, U, V> = Transformer<Mutable, T, U, V> */
declare type MTransformer<T, U, V> = Transformer<Mutable, T, U, V>;
/*@ type MTransducer<A,B,C,T,U,V> = (MTransformer<A,B,C>)=>MTransformer<T,U,V> */
/*@ type MQQ<T> = QQ<Mutable,T> */
declare type MQQ<T> = QQ<Mutable,T>;

interface Pair<M extends ReadOnly, A, B> { x: A; y: B; }

declare type Entry<M extends ReadOnly> = Pair<M, string, any>;
/*@ type MMap<T> = (Mutable){[s:string]:T} */
declare type MMap<T> = {[s:string]:T};

interface Goog<M extends ReadOnly> {
    /*@ typeOf <M, T> (x:Array<M,T>) : {string | v =  "array"} */
    /*@ typeOf (x:top)               : {string | v != "array"} */
    typeOf(x:any):string;
}
declare let goog:Goog<Immutable>;

interface IterResult<M extends ReadOnly> {
    done:boolean;
    value:any;
}
class EmptyObject<M extends ReadOnly> {
    constructor() { }
}
interface Iterator<M extends ReadOnly> extends EmptyObject<M> {
    next():IterResult<M>;
}
interface Iterable<M extends ReadOnly> extends EmptyObject<M> {
    ATATiterator():Iterator<M>;
}
interface TruncatedTransformer<M extends ReadOnly, IN, INTER> {
    init:()=>INTER;
    /*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
    step:(result:INTER, input:IN)=>MQQ<INTER>;
}
interface Transformer<M extends ReadOnly, IN, INTER, OUT> extends TruncatedTransformer<M, IN, INTER> {
    /*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
    result:(result:MQQ<INTER>)=>OUT;
}

        // "use strict";

        // goog.provide("com.cognitect.transducers");

// =============================================================================
// Build target config

/** @define {boolean} */
/*@ readonly */
let TRANSDUCERS_DEV = true;

/** @define {boolean} */
/*@ readonly */
let TRANSDUCERS_NODE_TARGET = false;

/** @define {boolean} */
/*@ readonly */
let TRANSDUCERS_BROWSER_TARGET = false;

/** @define {boolean} */
/*@ readonly */
let TRANSDUCERS_BROWSER_AMD_TARGET = false;

        // goog.scope(function() {

        // /**
        //  * @class transducers
        //  */
        // let transducers = com.cognitect.transducers;

// =============================================================================
// Utilities

/*@ isString :: (x:top) => {boolean | (Prop v) <=> (ttag(x) = "string")} */
function isString(x:any) {
    return typeof x === "string";
}

        //TODO
        // if(typeof Array.isArray != "undefined") {
        //     transducers.isArray = function(x) {
        //         return Array.isArray(x);
        //     }
        // } else {
/*@ isArray :: <M, T> (x:Array<M,T>) => {boolean | Prop v} */
/*@ isArray :: (x:top) => {boolean | not (Prop v)} */
function isArray(x:any) {
    return goog.typeOf(x) === "array";
}
        // }

function isObject(x:any) {
    return goog.typeOf(x) === "object";
}

/*@ isIterable :: <T> (x:ObjectK) => {boolean | Prop(v) <=> IsIter(x)} */
function isIterable(x:any) {
    assume(false);
    return true;
    // PORT TODO:
    return ("ATATiterator" in x) || ("next" in x);
}

        // NOTICE: this seems inherently not typesafe and thus impossible to support
        // transducers.slice = function(arrayLike, start, n) {
        //     if(n == null) {
        //         return Array.prototype.slice.call(arrayLike, start);
        //     } else {
        //         return Array.prototype.slice.call(arrayLike, start, n);
        //     }
        // };

/**
 * Take a predicate function and return its complement.
 * @method transducers.complement
 * @param {function} a predicate function
 * @return {function} the complement predicate function
 * @example
 *     let isEven = function(n) { return n % 2 == 0; };
 *     let isOdd = transducers.complement(isEven);
 */
//TODO: this now only supports unary functions
/*@ complement :: <T> ((T)=>top) => (T) => {boolean | 0 < 1} */
function complement(f) {
    /*@ readonly */
    let ff = f;
    return function(y)
    /*@ <anonymous> (T) => {boolean | 0 < 1} */
    { return !ff(y) };
}

class Wrap<M extends ReadOnly, IN, OUT> implements Transformer<M, IN, OUT, OUT> {
    /*@ stepFn : (OUT, IN)=>MQQ<OUT> */
    public stepFn: (result:OUT, input:IN)=>MQQ<OUT>;
    /*@ new (stepFn:(result:OUT, input:IN)=>MQQ<OUT>) : {Wrap<M, IN, OUT> | 0 < 1} */
    constructor(stepFn:(result:OUT, input:IN)=>MQQ<OUT>) {
        this.stepFn = stepFn;
    }

    init():OUT {
        throw new Error("init not implemented");
    }
    /*@ result (result:MQQ<OUT>) : {OUT | 0 < 1} */
    result(result:MQQ<OUT>):OUT {
        return result.value; //TODO: to maintain the original generality this should actually just be 'result' and the return value is then QQ<OUT>
    }
    /*@ step (result:OUT, input:IN) : {MQQ<OUT> | 0 < 1} */
    step(result:OUT, input:IN):MQQ<OUT> {
        return this.stepFn(result, input);
    }
}

/**
 * Take a two-arity reducing function where the first argument is the
 * accumluation and the second argument is the next input and convert
 * it into a transducer transformer object.
 * @method transducers.wrap
 * @param {function} stepFn a two-arity reducing function
 * @return {transducers.Wrap} a transducer transformer object
 * @example
 *     let t = transducers;
 *     let arrayPush = t.wrap(function(arr, x) { arr.push(x); return arr; });
 */
/*@ wrap :: <IN, OUT, M> (stepFn: (result:OUT, input:IN)=>OUT) => {Wrap<M, IN, OUT> | 0 < 1} */
/*@ wrap :: <IN, T, OUT> (stepFn: MTransformer<IN, T, OUT>) => {MTransformer<IN, T, OUT> | 0 < 1} */
function wrap<IN, OUT>(stepFn:any) {
    if(typeof stepFn === "function") {
        return generalWrap(addQQ0(stepFn));
    } else {
        return stepFn;
    }
}

/*@ addQQ0 :: <M, T, U> (stepFn:(T,U)=>T) => (T,U) => {QQ<M,T> | 0 < 1} */
function addQQ0<T, U>(stepFn:(T,U)=>T):(T,U)=>MQQ<T> {
    /*@ readonly */
    let ff = stepFn;
    return function(t:T, u:U)
    /*@ <anonymous> (T,U)=>{QQ<M,T> | 0 < 1} */
    { return new QQ(ff(t,u), 0) };
}

/*@ generalWrap :: <IN, OUT, M> (stepFn: (result:OUT, input:IN)=>MQQ<OUT>) => {Wrap<M, IN, OUT> | 0 < 1} */
/*@ generalWrap :: <IN, T, OUT> (stepFn: MTransformer<IN, T, OUT>) => {MTransformer<IN, T, OUT> | 0 < 1} */
function generalWrap(stepFn:any) {
    if(typeof stepFn === "function") {
        return new Wrap(stepFn);
    } else {
        return stepFn;
    }
}

// =============================================================================
// Main

class QQ<M extends ReadOnly, T> {
    /*@ __transducers_reduced__ : number */
    public __transducers_reduced__:number;
    public value:T;

    /*@ new (value:T, reducedCount:number) : {QQ<M, T> | 0 < 1} */
    constructor(value:T, reducedCount:number) {
        this.__transducers_reduced__ = reducedCount;
        this.value = value;
    }
}

        /**
         * Return a reduced value. Reduced values short circuit transduce.
         * @method transducers.reduced
         * @param {Object} x any JavaScript value
         * @return {transducers.Reduced} a reduced value
         * @example
         *     let reduced = transducers.reduced(1);
         */
        // TODO
        // /*@ reduced :: <T, M> (T) => {QQ<M, T> | 0 < 1} */
        // function reduced<T>(x:T) {
        //     return new QQ(x, true);
        // }

/**
 * Check if a value is reduced.
 * @method transducers.isReduced
 * @param {Object} x any JavaScript value
 * @return {Boolean} true if the value is an instance of transducers.Reduced
 *   false otherwise
 * @example
 *     let t = transducers;
 *     t.isReduced(1); // false
 *     t.isReduced(t.reduced(1)); // true
 */
/*@ isReduced :: <T> (x:QQ<ReadOnly, T>) => {boolean | 0 < 1} */
function isReduced<T>(x:QQ<ReadOnly, T>) {
    return (x.__transducers_reduced__ > 0); //TODO:(x instanceof Reduced || (x && x.__transducers_reduced__);
}

        // NOTICE: ensureReduced and unreduced removed as irrelevant to the new formulation of Reduced as QQ

        //TODO
        // /*@ deref :: <T, M> (QQ<M, T>) => {T | 0 < 1} */
        // function deref<T>(x:QQ<T>):T {
        //     return x.value;
        // }

/**
 * Identity function.
 * @method transducers.identiy
 * @param {Object} x any JavaScript value
 * @return {Object} a JavaScript value
 * @example
 *     transducers.identity(1); // 1
 */
/*@ identity :: <T> (x:T) => {T | v = x} */
function identity<T>(x:T):T {
    return x;
}

/**
 * Function composition. Take N function and return their composition.
 * @method transducers.comp
 * @param {Function} varArgs N functions
 * @result {Function} a function that represent the composition of the arguments.
 * @example
 *     let t = transducers;
 *     let inc = function(n) { return n + 1 };
 *     let double = function(n) { return n * 2 };
 *     let incDouble = t.comp(double, inc);
 *     incDouble(3); // 8
 */
//TODO
/*@ comp :: <S, T, U> (f:(T)=>U, g:(S)=>T) => (S) => {U | 0 < 1} */
/* PORT TODO: comp :: <T> (f:(T)=>T, g:{IArray<(T)=>T> | (len g) > 0}) => (T) => {T | 0 < 1} */
function comp(f:Function, g:any) {
    if (typeof g === "function") {
        return binaryComp(f,g);
    } else {
        return reduce(binaryComp, f, g);
    }
}

/*@ binaryComp :: <S, T, U> (f:(T)=>U, g:(S)=>T) => (S) => {U | 0 < 1} */
function binaryComp(f, g) {
    /*@ readonly */
    let ff = f;
    /*@ readonly */
    let gg = g;
    return function(s)
    /*@ <anonymous> (S)=>{U | 0 < 1} */
    { return ff(gg(s)) }
}

class Map<M extends ReadOnly, IN, INTER, OUT, T> implements Transformer<M, IN, INTER, OUT> {
public f: (z:IN) => T;
/*@ xf : MTransformer<T, INTER, OUT> */
public xf: MTransformer<T, INTER, OUT>;
/*@ new (f:(IN) => T, xf:MTransformer<T, INTER, OUT>) : {Map<M, IN, INTER, OUT, T> | 0 < 1} */
constructor(f:(z:IN) => T, xf:MTransformer<T, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    return this.xf.step(result, this.f(input));
}
}

/**
 * Mapping transducer constructor
 * @method transducers.map
 * @param {Function} f the mapping operation
 * @return {transducers.Map} returns a mapping transducer
 * @example
 *     let t = transducers;
 *     let inc = function(n) { return n+1; };
 *     let xf = t.map(inc);
 *     t.into([], xf, [1,2,3]); // [2,3,4]
 */
/*@ map :: <IN, INTER, OUT, T, M> (f: (IN)=>T) => (xf: MTransformer<T, INTER, OUT>) => {Map<M, IN, INTER, OUT, T> | 0 < 1} */
function map<M extends ReadOnly, IN, INTER, OUT, T>(f: (IN)=>T): (xf: MTransformer<T, INTER, OUT>) => Map<M, IN, INTER, OUT, T> {
    /*@ readonly */
    let ff = f;
    if(TRANSDUCERS_DEV && (f === null)) {
        throw new Error("At least one argument must be supplied to map");
    } else {
        return function(xf)
            /*@ <anonymous> <M> (xf: MTransformer<T, INTER, OUT>) => {Map<M, IN, INTER, OUT, T> | 0 < 1} */
            { return new Map(ff, xf); };
    }
}

class Filter<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (pred:(IN) => boolean, xf:MTransformer<IN, INTER, OUT>) : {Filter<M, IN, INTER, OUT> | 0 < 1} */
constructor(pred: (z:IN) => boolean, xf: MTransformer<IN, INTER, OUT>) {
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    if(this.pred(input)) {
        return this.xf.step(result, input);
    } else {
        return new QQ(result, 0);
    }
}
}

/**
 * Filtering transducer constructor
 * @method transducers.filter
 * @param {Function} pred a predicate function
 * @return {transducers.Filter} returns a filtering transducer
 * @example
 *     let t = transducers;
 *     let isEven = function(n) { return n % 2 == 0; };
 *     let xf = t.filter(isEven);
 *     t.into([], xf, [0,1,2,3,4]); // [0,2,4];
 */
/*@ filter :: <IN, INTER, OUT, M> (pred: (IN)=>boolean) => (xf: MTransformer<IN, INTER, OUT>) => {Filter<Unique, IN, INTER, OUT> | 0 < 1} */
function filter<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: MTransformer<IN, INTER, OUT>) => Filter<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("filter must be given a function");
    } else {
        return function(xf)
        /*@ <anonymous> <M> (MTransformer<IN, INTER, OUT>) => {Filter<M, IN, INTER, OUT> | 0 < 1} */
        { return new Filter(ff, xf) };
    }
}

/**
 * Similar to filter except the predicate is used to
 * eliminate values.
 * @method transducers.remove
 * @param {Function} pred a predicate function
 * @return {transducers.Filter} returns a removing transducer
 * @example
 *     let t = transducers;
 *     let isEven = function(n) { return n % 2 == 0; };
 *     let xf = t.remove(isEven);
 *     t.into([], xf, [0,1,2,3,4]); // [1,3];
 */
/*@ remove :: <IN, INTER, OUT, M> (pred: (IN)=>boolean) => (xf: MTransformer<IN, INTER, OUT>) => {Filter<M, IN, INTER, OUT> | 0 < 1} */
function remove<M extends ReadOnly, IN, INTER, OUT>(pred: (IN)=>boolean): (xf: MTransformer<IN, INTER, OUT>) => Filter<M, IN, INTER, OUT> {
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("remove must be given a function");
    } else {
        return filter<IN, INTER, OUT>(complement(pred));
    }
}

class Take<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ n : number */
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (n:number, xf:MTransformer<IN, INTER, OUT>) : {Take<M, IN, INTER, OUT> | 0 < 1} */
constructor(n:number, xf:MTransformer<IN, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    let retval;
    if(this.n > 0) {
        retval = this.xf.step(result, input);
    } else {
        retval = new QQ(result, 1); //TODO: modified semantics here...
    }
    this.n--;
    return retval;
}
}

/**
 * A take transducer constructor. Will take n values before
 * returning a reduced result.
 * @method transducers.take
 * @param {Number} n the number of inputs to receive.
 * @return {transducers.Take} a take transducer
 * @example
 *     let t = transducers;
 *     let xf = t.take(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [0,1,2];
 */
/*@ take :: <IN, INTER, OUT, M> (n:number) => (xf: MTransformer<IN, INTER, OUT>) => {Take<M, IN, INTER, OUT> | 0 < 1} */
function take<M extends ReadOnly, IN, INTER, OUT>(n:number): (xf: MTransformer<IN, INTER, OUT>) => Take<M, IN, INTER, OUT> {
    /*@ readonly */
    let nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("take must be given an integer");
    } else {
        return function(xf)
        /*@ <anonymous> <M> (MTransformer<IN, INTER, OUT>) => {Take<M, IN, INTER, OUT> | 0 < 1} */
        { return new Take(nn, xf) };
    }
}

class TakeWhile<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (pred:(z:IN) => boolean, xf:MTransformer<IN, INTER, OUT>) : {TakeWhile<M, IN, INTER, OUT> | 0 < 1} */
constructor(pred: (z:IN) => boolean, xf: MTransformer<IN, INTER, OUT>) {
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    if(this.pred(input)) {
        return this.xf.step(result, input);
    } else {
        return new QQ(result, 1);
    }
}
}

/**
 * Like the take transducer except takes as long as the pred
 * return true for inputs.
 * @method transducers.takeWhile
 * @param {Function} pred a predicate function
 * @return {transducers.TakeWhile} a takeWhile transducer
 * @example
 *     let t = transducers;
 *     let xf = t.takeWhile(function(n) { return n < 3; });
 *     t.into([], xf, [0,1,2,3,4,5]); // [0,1,2];
 */
/*@ takeWhile :: <IN, INTER, OUT, M> (pred: (IN)=>boolean) => (xf: MTransformer<IN, INTER, OUT>) => {TakeWhile<M, IN, INTER, OUT> | 0 < 1} */
function takeWhile<M extends ReadOnly, IN, INTER, OUT>(pred: (IN)=>boolean): (xf: MTransformer<IN, INTER, OUT>) => TakeWhile<M, IN, INTER, OUT> {
    /*@ readonly */
    let ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("takeWhile must given a function");
    } else {
        return function(xf)
        /*@ <anonymous> <M> (MTransformer<IN, INTER, OUT>) => {TakeWhile<M, IN, INTER, OUT> | 0 < 1} */
        { return new TakeWhile(ff, xf) };
    }
}

class TakeNth<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ i : number */
public i: number;
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (n:number, xf:MTransformer<IN, INTER, OUT>) : {TakeNth<M, IN, INTER, OUT> | 0 < 1} */
constructor(n:number, xf:MTransformer<IN, INTER, OUT>) {
    this.i = -1;
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    this.i++;
    if (true) {//PORT TODO ((this.i % this.n) === 0) {
        return this.xf.step(result, input);
    } else {
        return new QQ(result, 0);
    }
}
}

/**
 * A transducer that takes every Nth input
 * @method transducers.takeNth
 * @param {Number} n an integer
 * @return {transducers.TakeNth} a takeNth transducer
 * @example
 *     let t = transducers;
 *     let xf = t.takeNth(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [2,5];
 */
/*@ takeNth :: <IN, INTER, OUT, M> (n:number) => (xf: MTransformer<IN, INTER, OUT>) => {TakeNth<Unique, IN, INTER, OUT> | 0 < 1} */
function takeNth<IN, INTER, OUT>(n:number): (xf: MTransformer<IN, INTER, OUT>) => TakeNth<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("takeNth must be given a number");
    } else {
        return function(xf)
        /*@ <anonymous> <M> (MTransformer<IN, INTER, OUT>) => {TakeNth<M, IN, INTER, OUT> | 0 < 1} */
        { return new TakeNth(nn, xf) };
    }
}

// no maps in JS, perhaps define only if transit or
// Map available? - David

class Drop<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ n : number */
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (n:number, xf:MTransformer<IN, INTER, OUT>) : {Drop<M, IN, INTER, OUT> | 0 < 1} */
constructor(n:number, xf:MTransformer<IN, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    if(this.n > 0) {
        this.n--;
        return new QQ(result, 0);
    } else {
        return this.xf.step(result, input);
    }
}
}

/**
 * A dropping transducer constructor
 * @method transducers.drop
 * @param {Number} n an integer, the number of inputs to drop.
 * @return {transducers.Drop} a dropping transducer
 * @example
 *     let t = transducers;
 *     let xf = t.drop(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [3,4,5];
 */
/*@ drop :: <IN, INTER, OUT> (n:number) => (xf: MTransformer<IN, INTER, OUT>) => {Drop<Unique, IN, INTER, OUT> | 0 < 1} */
function drop<IN, INTER, OUT>(n:number): (xf: MTransformer<IN, INTER, OUT>) => Drop<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("drop must be given an integer");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<IN, INTER, OUT>) => {Drop<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new Drop(nn, xf) };
    }
}

class DropWhile<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ drop : boolean */
public drop: boolean;
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (pred:(z:IN) => boolean, xf:MTransformer<IN, INTER, OUT>) : {DropWhile<M, IN, INTER, OUT> | 0 < 1} */
constructor(pred: (z:IN) => boolean, xf: MTransformer<IN, INTER, OUT>) {
    this.drop = true;
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    if(this.drop && this.pred(input)) {
        return new QQ(result, 0);
    } else {
        if(this.drop) this.drop = false;
        return this.xf.step(result, input);
    }
}
}

/**
 * A dropping transducer that drop inputs as long as
 * pred is true.
 * @method transducers.dropWhile
 * @param {Function} pred a predicate function
 * @return {transducers.DropWhile} a dropWhile transducer
 * @example
 *     let t = transducers;
 *     let xf = t.dropWhile(function(n) { return n < 3; });
 *     t.into([], xf, [0,1,2,3,4,5]); // [3,4,5];
 */
/*@ dropWhile :: <IN, INTER, OUT, M> (pred: (IN)=>boolean) => (xf: MTransformer<IN, INTER, OUT>) => {DropWhile<Unique, IN, INTER, OUT> | 0 < 1} */
function dropWhile<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: MTransformer<IN, INTER, OUT>) => DropWhile<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("dropWhile must be given a function");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<IN, INTER, OUT>) => {DropWhile<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new DropWhile(ff, xf) };
    }
}

/*@ readonly */
let NONE = {};

class PartitionBy<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
public f: (z:IN) => any;
/*@ xf : MTransformer<MArray<IN>, INTER, OUT> */
public xf: MTransformer<MArray<IN>, INTER, OUT>;
/*@ a : MArray<IN> */
public a: MArray<IN>;
/*@ pval : top */
public pval: any;
/*@ new (f:(IN) => top, xf:MTransformer<MArray<IN>, INTER, OUT>) : {PartitionBy<M, IN, INTER, OUT> | 0 < 1} */
constructor(f: (z:IN) => any, xf: MTransformer<MArray<IN>, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
    this.a = [];
    this.pval = NONE;
}

init():INTER {
    return this.xf.init();
}
/*@ @Mutable result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    if(this.a.length > 0) {
        result = this.xf.step(result.value, this.a);
        result.__transducers_reduced__ = 0;
        this.a = [];
    }
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    let pval = this.pval;
    let val  = this.f(input);

    this.pval = val;

    // NOTE: we should probably allow someone to define
    // equality? - David
    if((pval === NONE) ||
       (pval === val)) {
        this.a.push(input);
        return new QQ(result, 0);
    } else {
        let ret = this.xf.step(result, this.a);
        this.a = [];
        if(!isReduced(ret)) {
            this.a.push(input);
        }
        return ret;
    }
}
}

/**
 * A partitioning transducer. Collects inputs into
 * arrays as long as predicate remains true for contiguous
 * inputs.
 * @method transducers.partitionBy
 * @param {Function} f a partition function. When the result
 *   for an input changes from the previous result will create
 *   a partition.
 * @return {transducers.PartitionBy} a partitionBy transducer
 * @example
 *     let t = transducers;
 *     let xf = t.partitionBy(function(x) { return typeof x == "string"; });
 *     t.into([], xf, [0,1,"foo","bar",2,3,"bar","baz"]); // [[0,1],["foo","bar"],[2,3],["bar","baz"]];
 */
/*@ partitionBy :: <IN, INTER, OUT, M> (f: (IN)=>top) => (xf: MTransformer<MArray<IN>, INTER, OUT>) => {PartitionBy<Unique, IN, INTER, OUT> | 0 < 1} */
function partitionBy<IN, INTER, OUT>(f: (IN)=>any): (xf: MTransformer<MArray<IN>, INTER, OUT>) => PartitionBy<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("partitionBy must be given an function");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<MArray<IN>, INTER, OUT>) => {PartitionBy<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new PartitionBy(ff, xf) };
    }
}

class PartitionAll<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
public n: number;
/*@ xf : MTransformer<MArray<IN>, INTER, OUT> */
public xf: MTransformer<MArray<IN>, INTER, OUT>;
/*@ a : MArray<IN> */
public a: MArray<IN>;
/*@ new (n:number, xf:MTransformer<MArray<IN>, INTER, OUT>) : {PartitionAll<M, IN, INTER, OUT> | 0 < 1} */
constructor(n:number, xf:MTransformer<MArray<IN>, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
    this.a = [];
}

init():INTER {
    return this.xf.init();
}
/*@ @Mutable result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    if(this.a.length > 0) {
        result = this.xf.step(result.value, this.a);
        result.__transducers_reduced__ = 0;
        this.a = [];
    }
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    this.a.push(input);
    if(this.n === this.a.length) {
        let a = this.a;
        this.a = [];
        return this.xf.step(result, a);
    } else {
        return new QQ(result, 0);
    }
}
}

/**
 * A partitioning transducer. Collects inputs into
 * arrays of size N.
 * @method transducers.partitionAll
 * @param {Number} n an integer
 * @return {transducers.PartitionAll} a partitionAll transducer
 * @example
 *     let t = transducers;
 *     let xf = t.partitionAll(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [[0,1,2],[3,4,5]]
 */
/*@ partitionAll :: <IN, INTER, OUT, M> (n:number) => (xf: MTransformer<MArray<IN>, INTER, OUT>) => {PartitionAll<Unique, IN, INTER, OUT> | 0 < 1} */
function partitionAll<IN, INTER, OUT>(n:number): (xf: MTransformer<MArray<IN>, INTER, OUT>) => PartitionAll<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("partitionAll must be given a number");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<MArray<IN>, INTER, OUT>) => {PartitionAll<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new PartitionAll(nn, xf) };
    }
}

class Keep<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
public f: (z:IN) => any;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (f:(IN) => top, xf:MTransformer<IN, INTER, OUT>) : {Keep<M, IN, INTER, OUT> | 0 < 1} */
constructor(f: (z:IN) => any, xf: MTransformer<IN, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    let v = this.f(input);
    if(v === null) {
        return new QQ(result, 0);
    } else {
        return this.xf.step(result, input);
    }
}
}

/**
 * A keeping transducer. Keep inputs as long as the provided
 * function does not return null or undefined.
 * @method transducers.keep
 * @param {Function} f a function
 * @return {transducers.Keep} a keep transducer
 * @example
 *     let t = transducers;
 *     let xf = t.keep(function(x) { if(typeof x == "string") return "cool"; });
 *     t.into([], xf, [0,1,"foo",3,4,"bar"]); // ["foo","bar"]
 */
/*@ keep :: <IN, INTER, OUT, M> (f: (IN)=>top) => (xf: MTransformer<IN, INTER, OUT>) => {Keep<Unique, IN, INTER, OUT> | 0 < 1} */
function keep<IN, INTER, OUT>(f: (IN)=>any): (xf: MTransformer<IN, INTER, OUT>) => Keep<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("keep must be given a function");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<IN, INTER, OUT>) => {Keep<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new Keep(ff, xf) };
    }
}

class KeepIndexed<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ i : number */
public i: number;
public f: (idx:number, z:IN) => any;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ new (f:(idx:number, z:IN) => top, xf:MTransformer<IN, INTER, OUT>) : {KeepIndexed<M, IN, INTER, OUT> | 0 < 1} */
constructor(f: (idx:number, z:IN) => any, xf: MTransformer<IN, INTER, OUT>) {
    this.i = -1;
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ @Mutable step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    this.i++;
    let v:any = this.f(this.i, input);
    if(v === null) {
        return new QQ(result, 0);
    } else {
        return this.xf.step(result, input);
    }
}
}

/**
 * Like keep but the provided function will be passed the
 * index as the first argument.
 * @method transducers.keepIndexed
 * @param {Function} f a function
 * @return {transducers.KeepIndexed} a keepIndexed transducer
 * @example
 *     let t = transducers;
 *     let xf = t.keepIndexed(function(i, x) { if(typeof x == "string") return "cool"; });
 *     t.into([], xf, [0,1,"foo",3,4,"bar"]); // ["foo","bar"]
 */
/*@ keepIndexed :: <IN, INTER, OUT, M> (f: (idx:number, z:IN)=>top) => (xf: MTransformer<IN, INTER, OUT>) => {KeepIndexed<Unique, IN, INTER, OUT> | 0 < 1} */
function keepIndexed<IN, INTER, OUT>(f: (idx:number, z:IN)=>any): (xf: MTransformer<IN, INTER, OUT>) => KeepIndexed<Unique, IN, INTER, OUT> {
    /*@ readonly */
    let ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("keepIndexed must be given a function");
    } else {
        return function(xf)
        /*@ <anonymous> (MTransformer<IN, INTER, OUT>) => {KeepIndexed<Unique, IN, INTER, OUT> | 0 < 1} */
        { return new KeepIndexed(ff, xf) };
    }
}

// randomSample
// iteration

/**
 * Given a transformer returns a transformer which preserves
 * reduced by wrapping one more time. See cat.
 * @method transducers.preservingReduced
 * @param {transformer} xf a transformer
 * @return {transformer} a transformer which preserves reduced
 */
/*@ preservingReduced :: <IN, INTER, OUT> (xf: MTransformer<IN, INTER, OUT>) => {MTransformer<IN, INTER, MQQ<INTER>> | 0 < 1} */
function preservingReduced<IN, INTER, OUT>(xf: MTransformer<IN, INTER, OUT>) {
    return new PreservingReduced(xf);
}

class PreservingReduced<M extends ReadOnly, IN, INTER> implements Transformer<M, IN, INTER, MQQ<INTER>> {
/*@ xf : TruncatedTransformer<Mutable, IN, INTER> */
public xf: TruncatedTransformer<Mutable, IN, INTER>;
/*@ new (xf:TruncatedTransformer<Mutable, IN, INTER>) : {PreservingReduced<M, IN, INTER> | 0 < 1} */
constructor(xf: TruncatedTransformer<Mutable, IN, INTER>) {
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {MQQ<INTER> | 0 < 1} */
result(result:MQQ<INTER>):MQQ<INTER> {
    return result;
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IN):MQQ<INTER> {
    let ret = this.xf.step(result, input);
    if(isReduced(ret)) ret.__transducers_reduced__++;
    return ret;
}
}


/**
 * Given a transformer return a concatenating transformer
 * @method transducers.cat
 * @param {transformer} xf a transformer
 * @return {transformer} a concatenating transformer
 */
/*@ cat :: <IN, INTER, OUT> (xf: MTransformer<IN, INTER, OUT>) => {MTransformer<IArray<IN>, INTER, OUT> | 0 < 1} */
function cat<IN, INTER, OUT>(xf: MTransformer<IN, INTER, OUT>) {
    return new Cat(xf);
}

class Cat<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IArray<IN>, INTER, OUT> {
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: MTransformer<IN, INTER, OUT>;
/*@ rxf : MTransformer<IN, INTER, MQQ<INTER>> */
public rxf: MTransformer<IN, INTER, MQQ<INTER>>;
/*@ new (xf:MTransformer<IN, INTER, OUT>) : {Cat<M, IN, INTER, OUT> | 0 < 1} */
constructor(xf: MTransformer<IN, INTER, OUT>) {
    this.xf = xf;
    this.rxf = preservingReduced(xf);
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step (result:INTER, input:IArray<IN>) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, input:IArray<IN>):MQQ<INTER> {
    return reduce(this.rxf, result, input);
}
}

/**
 * A mapping concatenating transformer
 * @method transducers.mapcat
 * @param {Function} f the mapping function
 * @return {Transducer} a mapping concatenating transducer
 * @example
 *     let t = transducers;
 *     let reverse = function(arr) { let arr = Array.prototype.slice.call(arr, 0); arr.reverse(); return arr; }
 *     let xf = t.mapcat(reverse);
 *     t.into([], xf, [[3,2,1],[6,5,4]]); // [1,2,3,4,5,6]
 */
/*@ mapcat :: <IN, INTER, OUT, S, M> (f: (z:S)=>IArray<IN>) => (xf: MTransformer<IN, INTER, OUT>) => {Map<M, S, INTER, OUT, IArray<IN>> | 0 < 1} */
function mapcat<IN, INTER, OUT, S>(f: (z:S)=>IN[]) {
    /*@ readonly */
    let ff = f;
    return function(xf: MTransformer<IN, INTER, OUT>)
    /*@ <anonymous> <M> (xf: MTransformer<IN, INTER, OUT>) => {Map<M, S, INTER, OUT, IArray<IN>> | 0 < 1} */
    {
        return map(ff)(cat(xf))
    }
}

function slen(x:string):number { return _pos(); }

/*@ stringReduce :: <INTER, OUT> (xf:MTransformer<string, INTER, OUT>, init:INTER, str:string) => {OUT | 0 < 1} */
function stringReduce<INTER, OUT>(xf:MTransformer<string, INTER, OUT>, init:INTER, str:string) {
    let acc = init;
    /*@ wrappedAcc :: MQQ<INTER> */
    let wrappedAcc = new QQ(acc, 0);
    let shouldBreak = false;
    for(let i = 0; i < slen(str); i++) { //PORT TODO: was str.length
        wrappedAcc = xf.step(acc, str.charAt(i));
        if (isReduced(wrappedAcc)) {
            wrappedAcc.__transducers_reduced__--;
            shouldBreak = true;
        } else {
            acc = wrappedAcc.value;
        }
    }
    return xf.result(wrappedAcc);
}

/*@ arrayReduce :: <IN, INTER, OUT> (xf:MTransformer<IN, INTER, OUT>, init:INTER, array:IArray<IN>) => {OUT | 0 < 1} */
function arrayReduce<IN, INTER, OUT>(xf:MTransformer<IN, INTER, OUT>, init:INTER, array:IN[]) {
    let acc = init;
    /*@ wrappedAcc :: MQQ<INTER> */
    let wrappedAcc = new QQ(acc, 0);
    let shouldBreak = false;
    for(let i = 0; i < array.length && !shouldBreak; i++) {
        wrappedAcc = xf.step(acc, array[i]);
        if (isReduced(wrappedAcc)) {
            wrappedAcc.__transducers_reduced__--;
            shouldBreak = true;
        } else {
            acc = wrappedAcc.value;
        }
    }
    return xf.result(wrappedAcc);
}

/*@ objectReduce :: <M, INTER, OUT> (xf:MTransformer<Entry<M>, INTER, OUT>, init:INTER, ob:EmptyObject<Immutable>) => {OUT | 0 < 1} */
function objectReduce<INTER, OUT>(xf:MTransformer<{}, INTER, OUT>, init:INTER, ob:{[key:string]:any}) {
    let acc = init;
    /*@ wrappedAcc :: MQQ<INTER> */
    let wrappedAcc = new QQ(acc, 0);
    let shouldBreak = false;
    for(let p in ob) {
        if(!shouldBreak && ob.hasOwnProperty(p)) {
            wrappedAcc = xf.step(acc, {x:p, y:ob[p]}); //ORIG: [p, obj[p]]);
            if (isReduced(wrappedAcc)) {
                wrappedAcc.__transducers_reduced__--;
                shouldBreak = true;
            } else {
                acc = wrappedAcc.value;
            }
        }
    }
    return xf.result(wrappedAcc);
}

/*@ iterableReduce :: <INTER, OUT> (xf:MTransformer<top, INTER, OUT>, init:INTER, iter:ObjIter) => {OUT | 0 < 1} */
function iterableReduce(xf, init, iter) {
    let xiter;
    if("ATATiterator" in iter) {
        xiter = (<Iterable<ReadOnly>>iter).ATATiterator();
    } else {
        xiter = <Iterator<ReadOnly>>iter;
    }

    let acc = init;
    /*@ wrappedAcc :: MQQ<INTER> */
    let wrappedAcc = new QQ(acc, 0);
    let shouldBreak = false;
    let step = xiter.next();
    while(!shouldBreak) {
        wrappedAcc = xf.step(acc, step.value);
        if(isReduced(wrappedAcc)) {
            wrappedAcc.__transducers_reduced__--;
            shouldBreak = true;
        } else {
            step = xiter.next();
            shouldBreak = step.done;
            acc = wrappedAcc.value;
        }
    }

    return xf.result(wrappedAcc);
}

/**
 * Given a transducer, an intial value and a
 * collection - returns the reduction.
 * @method transducers.reduce
 * @param {Transducer|Function} xf a transducer or two-arity function
 * @param {Object} init any JavaScript value
 * @param {String|Array|Object|Iterable} coll any iterable JavaScript value
 * @return {Object} a iterable JavaScript value: string, array
 *   iterable, or object.
 */
// TODO: removed the if(coll) check but it wasn't sound anyway - e.g. it would reject coll==""
/*@ reduce :: <IN, INTER, OUT   > (xf: MTransformer<IN, INTER, OUT>,          init:INTER, coll:IArray<IN>) => {OUT | 0 < 1} */
/*@ reduce :: <    INTER, OUT   > (xf: MTransformer<string, INTER, OUT>,      init:INTER, coll:string)     => {OUT | 0 < 1} */
/*@ reduce :: <    INTER, OUT, M> (xf: MTransformer<Entry<M>, INTER, OUT>,    init:INTER, coll:ObjNoIter)  => {OUT | 0 < 1} */
/*@ reduce :: <    INTER, OUT   > (xf: MTransformer<top, INTER, OUT>,         init:INTER, coll:ObjIter)    => {OUT | 0 < 1} */
/*@ reduce :: <IN,        OUT   > (stepFn: (result:OUT, input:IN)=>OUT,       init:OUT,   coll:IArray<IN>) => {OUT | 0 < 1} */
/*@ reduce :: <           OUT   > (stepFn: (result:OUT, input:string)=>OUT,   init:OUT,   coll:string)     => {OUT | 0 < 1} */
/*@ reduce :: <           OUT, M> (stepFn: (result:OUT, input:Entry<M>)=>OUT, init:OUT,   coll:ObjNoIter)  => {OUT | 0 < 1} */
/*@ reduce :: <           OUT   > (stepFn: (result:OUT, input:top)=>OUT,      init:OUT,   coll:ObjIter)    => {OUT | 0 < 1} */
function reduce(xf:any, init:any, coll:any):any {
    xf = typeof xf === "function" ? wrap(xf) : xf;
    if(isString(coll)) {
        return stringReduce(xf, init, coll);
    } else if(isArray(coll)) {
        return arrayReduce(xf, init, coll);
    } else if(isIterable(coll)) {
        return iterableReduce(xf, init, coll);
    } else if(isObject(coll)) {
        return objectReduce(xf, init, coll);
    } else {
        throw new Error("Cannot reduce instance of ");// + coll.constructor.name); //TODO
    }
}

/**
 * Given a transducer, a builder function, an initial value
 * and a iterable collection - returns the reduction.
 * collection - returns the reduction.
 * @method transducers.transduce
 * @param {Transducer} xf a transducer
 * @param {Transducer|Function} f a transducer or two-arity function
 * @param {Object} init any JavaScript value
 * @param {String|Array|Object|Iterable} coll any iterable JavaScript value
 * @return {Object} a JavaScript value.
 * @example
 *     let t = transducers;
 *     let inc = function(n) { return n+1; };
 *     let isEven = function(n) { return n % 2 == 0; };
 *     let apush = function(arr,x) { arr.push(x); return arr; };
 *     let xf = t.comp(t.map(inc),t.filter(isEven));
 *     t.transduce(xf, apush, [], [1,2,3,4]); // [2,4]
 */
/*@ transduce :: <A,B,C,X,Y,Z  > (xf: MTransducer<A, B, C, X,        Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:IArray<X>) => {Z | 0 < 1} */
/*@ transduce :: <A,B,C,  Y,Z  > (xf: MTransducer<A, B, C, string,   Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:string)    => {Z | 0 < 1} */
/*@ transduce :: <A,B,C,  Y,Z,M> (xf: MTransducer<A, B, C, Entry<M>, Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:ObjNoIter) => {Z | 0 < 1} */
/*@ transduce :: <A,B,C,  Y,Z  > (xf: MTransducer<A, B, C, top,      Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:ObjIter)   => {Z | 0 < 1} */
/*@ transduce :: <A,  C,X,Y,Z  > (xf: MTransducer<A, C, C, X,        Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:IArray<X>) => {Z | 0 < 1} */
/*@ transduce :: <A,  C,  Y,Z  > (xf: MTransducer<A, C, C, string,   Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:string)    => {Z | 0 < 1} */
/*@ transduce :: <A,  C,  Y,Z,M> (xf: MTransducer<A, C, C, Entry<M>, Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:ObjNoIter) => {Z | 0 < 1} */
/*@ transduce :: <A,  C,  Y,Z  > (xf: MTransducer<A, C, C, top,      Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:ObjIter)   => {Z | 0 < 1} */
function transduce(xf:any, f:any, init:any, coll:any) {
    f = typeof f === "function" ? wrap(f) : f;
    xf = xf(f);
    return reduce(xf, init, coll);
}

// TODO: should be (string, string + number + boolean) => string
/*@ stringAppend :: (string, string) => {string | 0 < 1} */
function stringAppend(s, x) {
    return s + x;
}

/*@ arrayPush :: <T> (arr:MArray<T>, x:T) => {MArray<T> | 0 < 1} */
function arrayPush<T>(arr:T[], x:T) {
    arr.push(x);
    return arr;
}

/*@ addEntry :: <M extends ReadOnly, T> (ob: MMap<T>, entry: Pair<M,string,T>) => { MMap<T> | 0 < 1 } */
function addEntry(ob, entry) {
    ob[entry.x] = entry.y; //ORIG: ob[entry[0]] = entry[1];
    return ob;
}

/**
 * Reduce a value into the given empty value using a transducer.
 * @method transducers.into
 * @param {String|Array|Object} empty a JavaScript collection
 * @param {Transducer} xf a transducer
 * @param {Iterable} coll any iterable JavaScript value: array, string,
 *   object, or iterable.
 * @return {Object} a JavaScript value.
 * @example
 *     let t = transducers;
 *     let inc = function(n) { return n+1; };
 *     let isEven = function(n) { return n % 2 == 0; };
 *     let apush = function(arr,x) { arr.push(x); return arr; };
 *     let xf = t.comp(t.map(inc),t.filter(isEven));
 *     t.into([], xf, [1,2,3,4]); // [2,4]
 */
//TODO: when empty is a string, xf should actually be (MTransformer<string + number + boolean, string, string>) =>...
/*@ into :: <    Z    > (empty: string,    xf: MTransducer<string, string, string,             string,   string,    Z>, coll: string)    => {Z | 0 < 1} */
/*@ into :: <  X,Z    > (empty: string,    xf: MTransducer<string, string, string,             X,        string,    Z>, coll: IArray<X>) => {Z | 0 < 1} */
/*@ into :: <    Z,  M> (empty: string,    xf: MTransducer<string, string, string,             Entry<M>, string,    Z>, coll: ObjNoIter) => {Z | 0 < 1} */
/*@ into :: <    Z    > (empty: string,    xf: MTransducer<string, string, string,             top,      string,    Z>, coll: ObjIter)   => {Z | 0 < 1} */
/*@ into :: <T,  Z    > (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            string,   MArray<T>, Z>, coll: string)    => {Z | 0 < 1} */
/*@ into :: <T,X,Z    > (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            X,        MArray<T>, Z>, coll: IArray<X>) => {Z | 0 < 1} */
/*@ into :: <T,  Z,  M> (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            Entry<M>, MArray<T>, Z>, coll: ObjNoIter) => {Z | 0 < 1} */
/*@ into :: <T,  Z    > (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            top,      MArray<T>, Z>, coll: ObjIter)   => {Z | 0 < 1} */
/*@ into :: <T,  Z,N  > (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, string,   MMap<T>,   Z>, coll: string)    => {Z | 0 < 1} */
/*@ into :: <T,X,Z,N  > (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, X,        MMap<T>,   Z>, coll: IArray<X>) => {Z | 0 < 1} */
/*@ into :: <T,  Z,N,M> (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, Entry<M>, MMap<T>,   Z>, coll: ObjNoIter) => {Z | 0 < 1} */
/*@ into :: <T,  Z,N  > (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, top,      MMap<T>,   Z>, coll: ObjIter)   => {Z | 0 < 1} */
function into(empty, xf, coll) {
    if(isString(empty)) {
        return transduce(xf, stringAppend, empty, coll);
    } else if(isArray(empty)) {
        return transduce(xf, arrayPush, empty, coll);
    } else if(isObject(empty)) {
        return transduce(xf, addEntry, empty, coll);
    }
    else throw new Error("illegal 'empty' arg to into()");
}

class Completing<M extends ReadOnly, IN, INTER, OUT> implements Transformer<M, IN, INTER, OUT> {
/*@ cf : (z:MQQ<INTER>) => OUT */
public cf: (z:MQQ<INTER>) => OUT;
/*@ xf : TruncatedTransformer<Mutable, IN, INTER> */
public xf: TruncatedTransformer<Mutable, IN, INTER>;
/*@ new (cf:(z:MQQ<INTER>) => OUT, xf:TruncatedTransformer<Mutable, IN, INTER>) : {Completing<M, IN, INTER, OUT> | 0 < 1} */
constructor(cf: (z:MQQ<INTER>) => OUT, xf: TruncatedTransformer<Mutable, IN, INTER>) {
    this.cf = cf;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result (result:MQQ<INTER>) : {OUT | 0 < 1} */
result(result:MQQ<INTER>):OUT {
    return this.cf(result);
}
/*@ step (result:INTER, input:IN) : {MQQ<INTER> | 0 < 1} */
step(result:INTER, step:IN):MQQ<INTER> {
    return this.xf.step(result, step);
}
}

/**
 * A completing transducer constructor. Useful to provide cleanup
 * logic at the end of a reduction/transduction.
 * @method transducers.completing
 * @param {Transducer} xf a transducer
 * @param {Function} cf a function to apply at the end of the reduction/transduction
 * @return {Transducer} a transducer
 */
// TODO: the MTransformers in this signature could be TruncatedTransformers instead...
/*@ completing :: <IN, INTER, OUT, M, T> (xf: MTransformer<IN, INTER, T>,      cf: (MQQ<INTER>) => OUT) => {Completing<M, IN, INTER, OUT       > | 0 < 1} */
/*@ completing :: <IN, INTER, OUT, M>    (xf: (result:INTER, input:IN)=>INTER, cf: (MQQ<INTER>) => OUT) => {Completing<M, IN, INTER, OUT       > | 0 < 1} */
/*@ completing :: <IN, INTER, OUT, M, T> (xf: MTransformer<IN, INTER, T>)                               => {Completing<M, IN, INTER, MQQ<INTER>> | 0 < 1} */
/*@ completing :: <IN, INTER, OUT, M>    (xf: (result:INTER, input:IN)=>INTER)                          => {Completing<M, IN, INTER, MQQ<INTER>> | 0 < 1} */
function completing<IN, INTER, OUT>(xf: any, cf?: (z:MQQ<INTER>) => OUT):any {
    let wxf:TruncatedTransformer<Mutable, IN, INTER> = typeof xf === "function" ? wrap(xf) : xf;
    if(TRANSDUCERS_DEV && (wxf !== null) && !isObject(wxf)) {
        throw new Error("completing must be given a transducer as first argument");
    } else {
        if (cf) return new Completing(cf, wxf);
        return new Completing(identity, wxf);
    }
}

/**
 * Convert a transducer transformer object into a function so
 * that it can be used with existing reduce implementation i.e. native,
 * Underscore, lodash
 * @method transducers.toFn
 * @param {Transducer} xf a transducer
 * @param {Function} builder a function which take the accumulator and the
 *   the next input and return a new accumulator value.
 * @return {Function} a two-arity function compatible with existing reduce
 *   implementations
 * @example
 *     let t = transducers;
 *     let arr = [0,1,2,3,4,5],
 *     let apush = function(arr, x) { arr.push(x); return arr; },
 *     let xf = t.comp(t.map(inc),t.filter(isEven));
 *     arr.reduce(t.toFn(xf, apush), []); // [2,4,6]
 */
/*@ toFn :: <A, B, C, X, Y, T> (xf: MTransducer<A, B, C, X, Y, T>, builder: MTransformer<A, B, C>)  => (result:Y, input:X) => {MQQ<Y> | 0 < 1} */
/*@ toFn :: <A,    C, X, Y, T> (xf: MTransducer<A, C, C, X, Y, T>, builder: (result:C, input:A)=>C) => (result:Y, input:X) => {MQQ<Y> | 0 < 1} */
function toFn<IN, INTER, OUT>(xf, builder) {
    if(typeof builder === "function") {
        return xf(wrap(builder)).step;//TODO: see below
    }
    let rxf = xf(builder);
    return rxf.step//TODO: .bind(rxf);
}

// =============================================================================
// Utilities

/**
 * A transformer which simply returns the first input.
 * @method transducers.first
 * @return {Transducer} a transducer transformer
 */
let first:Wrap<Mutable, any, any> = generalWrap(function(result:any, input:any)
    /*@ <anonymous> (result:top, input:top) => {MQQ<top> | 0 < 1} */
    { return new QQ(input, 1); }
);

// =============================================================================
// Exporting
// NOTICE: this section was removed as irrelevant to the JS->RS port

}}} // end of module com.cognitect.transducers
