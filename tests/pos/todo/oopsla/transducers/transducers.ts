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

/*@ qualif Bot(s:Str,v:a): hasProperty(s,v) */
/*@ qualif Bot(s:Str,v:a): enumProp(s,v) */

module com {
module cognitect {
module transducers {

/*@ predicate Inst(X, Key, Type) = (hasProperty(Key, X) <=> extends_interface(X, Type)) */

/*@ predicate InstIterator(V) = Inst(V,"next","Iterator") */
/*@ predicate InstIterable(V) = Inst(V,"ATATiterator","Iterable") */

/*@ predicate IsIter(V) = extends_interface(V,"Iterator") || extends_interface(V,"Iterable") */

/*@ alias ObjectK = { v: EmptyObject<Immutable> | InstIterator(v) && InstIterable(v) } */
/*@ alias ObjIter   = { v: ObjectK |     IsIter(v) } */
/*@ alias ObjNoIter = { v: ObjectK | not IsIter(v) } */

/*@ alias MTransformer<T, U, V> = Transformer<Mutable, T, U, V> */
/*@ alias MTransducer<A,B,C,T,U,V> = (MTransformer<A,B,C>)=>MTransformer<T,U,V> */
/*@ alias MQQ<T> = QQ<Mutable,T> */

/*@ alias Entry<M> = Pair<M,string,top> */
/*@ alias MMap<T> = [Mutable]{[s:string]:T} */

interface Goog {
    /*@ typeOf : /\ forall M T . (x:Array<M,T>) : {string | v =  "array"}
                 /\              (x:top)        : {string | v != "array"} */
    typeOf(x:any):string;
}
declare var goog:Goog;

interface IterResult {
    done:boolean;
    value:any;
}
interface Iterator extends EmptyObject {
    next():IterResult;
}
interface Iterable extends EmptyObject {
    ATATiterator():Iterator;
}
interface TruncatedTransformer<IN, INTER> {
    init:()=>INTER;
    /*@ step : (result:INTER, input:IN) => {MQQ<INTER> | true} */
    step:(result:INTER, input:IN)=>QQ<INTER>;
}
interface Transformer<IN, INTER, OUT> extends TruncatedTransformer<IN, INTER> {
    /*@ result : (result:MQQ<INTER>) => {OUT | true} */
    result:(result:QQ<INTER>)=>OUT;
}

        // "use strict";

        // goog.provide("com.cognitect.transducers");

// =============================================================================
// Build target config

/** @define {boolean} */
/*@ readonly TRANSDUCERS_DEV :: # */ 
var TRANSDUCERS_DEV = true;

/** @define {boolean} */
/*@ readonly TRANSDUCERS_NODE_TARGET :: # */ 
var TRANSDUCERS_NODE_TARGET = false;

/** @define {boolean} */
/*@ readonly TRANSDUCERS_BROWSER_TARGET :: # */ 
var TRANSDUCERS_BROWSER_TARGET = false;

/** @define {boolean} */
/*@ readonly TRANSDUCERS_BROWSER_AMD_TARGET :: # */ 
var TRANSDUCERS_BROWSER_AMD_TARGET = false;

        // goog.scope(function() {

        // /**
        //  * @class transducers
        //  */
        // var transducers = com.cognitect.transducers;

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
/*@ isArray :: /\ forall M T . (x:Array<M,T>) => {boolean | Prop v}
               /\ (x:top) => {boolean | not (Prop v)} */
function isArray(x:any) {
    return goog.typeOf(x) === "array";
}
        // }

function isObject(x:any) {
    return goog.typeOf(x) === "object";
}

/*@ isIterable :: forall T . (x:ObjectK) => {boolean | Prop(v) <=> IsIter(x)} */
function isIterable(x:any) {
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
 *     var isEven = function(n) { return n % 2 == 0; };
 *     var isOdd = transducers.complement(isEven);
 */
//TODO: this now only supports unary functions
/*@ complement :: forall T . ((T)=>top) => {(T)=>boolean | true} */
function complement(f) {
    /*@ readonly ff :: # */ 
    var ff = f;
    return function(y)
    /*@ <anonymous> (T) => {boolean | true} */
    { return !ff(y) };
}

class Wrap<IN, OUT> implements Transformer<IN, OUT, OUT> {
    /*@ stepFn : (OUT, IN)=>MQQ<OUT> */
    public stepFn: (result:OUT, input:IN)=>QQ<OUT>;
    /*@ new(stepFn:(result:OUT, input:IN)=>MQQ<OUT>) => {Wrap<M, IN, OUT> | true} */
    constructor(stepFn:(result:OUT, input:IN)=>QQ<OUT>) {
        this.stepFn = stepFn;
    }

    init():OUT {
        throw new Error("init not implemented");
    }
    /*@ result : (result:MQQ<OUT>) : {OUT | true} */
    result(result:QQ<OUT>):OUT {
        return result.value; //TODO: to maintain the original generality this should actually just be 'result' and the return value is then QQ<OUT>
    }
    /*@ step : (result:OUT, input:IN) : {MQQ<OUT> | true} */
    step(result:OUT, input:IN):QQ<OUT> {
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
 *     var t = transducers;
 *     var arrayPush = t.wrap(function(arr, x) { arr.push(x); return arr; });
 */
/*@ wrap :: /\ forall IN OUT M . (stepFn: (result:OUT, input:IN)=>OUT) => {Wrap<M, IN, OUT> | true}
            /\ forall IN T OUT . (stepFn: MTransformer<IN, T, OUT>) => {MTransformer<IN, T, OUT> | true} */
function wrap<IN, OUT>(stepFn:any) {
    if(typeof stepFn === "function") {
        return generalWrap(addQQ0(stepFn));
    } else {
        return stepFn;
    }
}

/*@ addQQ0 :: forall M T U . (stepFn:(T,U)=>T) => {(T,U)=>QQ<M,T> | true} */
function addQQ0<T, U>(stepFn:(T,U)=>T):(T,U)=>QQ<T> {
    /*@ readonly ff :: # */ 
    var ff = stepFn;
    return function(t:T, u:U) 
    /*@ <anonymous> (T,U)=>{QQ<M,T> | true} */
    { return new QQ(ff(t,u), 0) };
}

/*@ generalWrap :: /\ forall IN OUT M . (stepFn: (result:OUT, input:IN)=>MQQ<OUT>) => {Wrap<M, IN, OUT> | true}
                   /\ forall IN T OUT . (stepFn: MTransformer<IN, T, OUT>) => {MTransformer<IN, T, OUT> | true} */
function generalWrap(stepFn:any) {
    if(typeof stepFn === "function") {
        return new Wrap(stepFn);
    } else {
        return stepFn;
    }
}

// =============================================================================
// Main

class QQ<T> {
    /*@ __transducers_reduced__ : number */
    public __transducers_reduced__:number;
    public value:T;

    /*@ new(value:T, reducedCount:number) => {QQ<M, T> | true} */
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
         *     var reduced = transducers.reduced(1);
         */
        // TODO
        // /*@ reduced :: forall T M . (T) => {QQ<M, T> | true} */
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
 *     var t = transducers;
 *     t.isReduced(1); // false
 *     t.isReduced(t.reduced(1)); // true
 */
/*@ isReduced :: forall T . (x:QQ<ReadOnly, T>) => {boolean | true} */
function isReduced<T>(x:QQ<T>) {
    return (x.__transducers_reduced__ > 0); //TODO:(x instanceof Reduced || (x && x.__transducers_reduced__);
}

        // NOTICE: ensureReduced and unreduced removed as irrelevant to the new formulation of Reduced as QQ

        //TODO
        // /*@ deref :: forall T M . (QQ<M, T>) => {T | true} */
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
/*@ identity :: forall T . (x:T) => {T | v = x} */
function identity<T>(x:T):T {
    return x;
}

/**
 * Function composition. Take N function and return their composition.
 * @method transducers.comp
 * @param {Function} varArgs N functions
 * @result {Function} a function that represent the composition of the arguments.
 * @example
 *     var t = transducers;
 *     var inc = function(n) { return n + 1 };
 *     var double = function(n) { return n * 2 };
 *     var incDouble = t.comp(double, inc);
 *     incDouble(3); // 8
 */
//TODO
/*@ comp :: /\ forall S T U . (f:(T)=>U, g:(S)=>T) => {(S)=>U | true}
            /\ forall T . (f:(T)=>T, g:{IArray<(T)=>T> | (len g) > 0}) => {(T)=>T | true} */
function comp(f:Function, g:any) {
    if (typeof g === "function") {
        return binaryComp(f,g);
    } else {
        return reduce(binaryComp, f, g);
    }
}

/*@ binaryComp :: forall S T U . (f:(T)=>U, g:(S)=>T) => {(S)=>U | true} */
function binaryComp(f, g) {
    /*@ readonly ff :: # */ 
    var ff = f;
    /*@ readonly gg :: # */ 
    var gg = g;
    return function(s) 
    /*@ <anonymous> (S)=>{U | true} */
    { return ff(gg(s)) }
}

class Map<IN, INTER, OUT, T> implements Transformer<IN, INTER, OUT> {
public f: (z:IN) => T;
/*@ xf : MTransformer<T, INTER, OUT> */
public xf: Transformer<T, INTER, OUT>;
/*@ new(f:(IN) => T, xf:MTransformer<T, INTER, OUT>) => {Map<M, IN, INTER, OUT, T> | true} */
constructor(f:(z:IN) => T, xf:Transformer<T, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    return this.xf.step(result, this.f(input));
}
}

/**
 * Mapping transducer constructor
 * @method transducers.map 
 * @param {Function} f the mapping operation
 * @return {transducers.Map} returns a mapping transducer
 * @example
 *     var t = transducers;
 *     var inc = function(n) { return n+1; };
 *     var xf = t.map(inc);
 *     t.into([], xf, [1,2,3]); // [2,3,4]
 */
/*@ map :: forall IN INTER OUT T M . (f: (IN)=>T) => {(xf: MTransformer<T, INTER, OUT>) => Map<M, IN, INTER, OUT, T> | true} */
function map<IN, INTER, OUT, T>(f: (IN)=>T): (xf: Transformer<T, INTER, OUT>) => Map<IN, INTER, OUT, T> {
    /*@ readonly ff :: # */ 
    var ff = f;
    if(TRANSDUCERS_DEV && (f === null)) {
        throw new Error("At least one argument must be supplied to map");
    } else {
        return function(xf) 
            /*@ <anonymous> forall M . (xf: MTransformer<T, INTER, OUT>) => {Map<M, IN, INTER, OUT, T> | true} */
            { return new Map(ff, xf); };
    }
}

class Filter<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(pred:(IN) => boolean, xf:MTransformer<IN, INTER, OUT>) => {Filter<M, IN, INTER, OUT> | true} */
constructor(pred: (z:IN) => boolean, xf: Transformer<IN, INTER, OUT>) {
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
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
 *     var t = transducers;
 *     var isEven = function(n) { return n % 2 == 0; };
 *     var xf = t.filter(isEven);
 *     t.into([], xf, [0,1,2,3,4]); // [0,2,4];
 */
/*@ filter :: forall IN INTER OUT M . (pred: (IN)=>boolean) => {(xf: MTransformer<IN, INTER, OUT>) => Filter<M, IN, INTER, OUT> | true} */
function filter<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: Transformer<IN, INTER, OUT>) => Filter<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("filter must be given a function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {Filter<M, IN, INTER, OUT> | true} */
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
 *     var t = transducers;
 *     var isEven = function(n) { return n % 2 == 0; };
 *     var xf = t.remove(isEven);
 *     t.into([], xf, [0,1,2,3,4]); // [1,3];
 */
/*@ remove :: forall IN INTER OUT M . (pred: (IN)=>boolean) => {(xf: MTransformer<IN, INTER, OUT>) => Filter<M, IN, INTER, OUT> | true} */
function remove<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: Transformer<IN, INTER, OUT>) => Filter<IN, INTER, OUT> {
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("remove must be given a function");
    } else {
        return filter<IN, INTER, OUT>(complement(pred));
    }
}

class Take<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ n : number */
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(n:number, xf:MTransformer<IN, INTER, OUT>) => {Take<M, IN, INTER, OUT> | true} */
constructor(n:number, xf:Transformer<IN, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (this:Take<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    var retval;
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
 *     var t = transducers;
 *     var xf = t.take(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [0,1,2];
 */
/*@ take :: forall IN INTER OUT M . (n:number) => {(xf: MTransformer<IN, INTER, OUT>) => Take<M, IN, INTER, OUT> | true} */
function take<IN, INTER, OUT>(n:number): (xf: Transformer<IN, INTER, OUT>) => Take<IN, INTER, OUT> {
    /*@ readonly nn :: # */ 
    var nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("take must be given an integer");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {Take<M, IN, INTER, OUT> | true} */
        { return new Take(nn, xf) };
    }
}

class TakeWhile<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(pred:(z:IN) => boolean, xf:MTransformer<IN, INTER, OUT>) => {TakeWhile<M, IN, INTER, OUT> | true} */
constructor(pred: (z:IN) => boolean, xf: Transformer<IN, INTER, OUT>) {
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
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
 *     var t = transducers;
 *     var xf = t.takeWhile(function(n) { return n < 3; });
 *     t.into([], xf, [0,1,2,3,4,5]); // [0,1,2];
 */
/*@ takeWhile :: forall IN INTER OUT M . (pred: (IN)=>boolean) => {(xf: MTransformer<IN, INTER, OUT>) => TakeWhile<M, IN, INTER, OUT> | true} */
function takeWhile<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: Transformer<IN, INTER, OUT>) => TakeWhile<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("takeWhile must given a function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {TakeWhile<M, IN, INTER, OUT> | true} */
        { return new TakeWhile(ff, xf) };
    }
}

class TakeNth<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ i : number */
public i: number;
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(n:number, xf:MTransformer<IN, INTER, OUT>) => {TakeNth<M, IN, INTER, OUT> | true} */
constructor(n:number, xf:Transformer<IN, INTER, OUT>) {
    this.i = -1;
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (this:TakeNth<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    this.i++;
    if((this.i % this.n) === 0) {
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
 *     var t = transducers;
 *     var xf = t.takeNth(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [2,5];
 */
/*@ takeNth :: forall IN INTER OUT M . (n:number) => {(xf: MTransformer<IN, INTER, OUT>) => TakeNth<M, IN, INTER, OUT> | true} */
function takeNth<IN, INTER, OUT>(n:number): (xf: Transformer<IN, INTER, OUT>) => TakeNth<IN, INTER, OUT> {
    /*@ readonly nn :: # */ 
    var nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("takeNth must be given a number");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {TakeNth<M, IN, INTER, OUT> | true} */
        { return new TakeNth(nn, xf) };
    }
}

// no maps in JS, perhaps define only if transit or
// Map available? - David

class Drop<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ n : number */
public n: number;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(n:number, xf:MTransformer<IN, INTER, OUT>) => {Drop<M, IN, INTER, OUT> | true} */
constructor(n:number, xf:Transformer<IN, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (this:Drop<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
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
 *     var t = transducers;
 *     var xf = t.drop(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [3,4,5];
 */
/*@ drop :: forall IN INTER OUT M . (n:number) => {(xf: MTransformer<IN, INTER, OUT>) => Drop<M, IN, INTER, OUT> | true} */
function drop<IN, INTER, OUT>(n:number): (xf: Transformer<IN, INTER, OUT>) => Drop<IN, INTER, OUT> {
    /*@ readonly nn :: # */ 
    var nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("drop must be given an integer");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {Drop<M, IN, INTER, OUT> | true} */
        { return new Drop(nn, xf) };
    }
}

class DropWhile<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ drop : boolean */
public drop: boolean;
public pred: (z:IN) => boolean;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(pred:(z:IN) => boolean, xf:MTransformer<IN, INTER, OUT>) => {DropWhile<M, IN, INTER, OUT> | true} */
constructor(pred: (z:IN) => boolean, xf: Transformer<IN, INTER, OUT>) {
    this.drop = true;
    this.pred = pred;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (this:DropWhile<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
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
 *     var t = transducers;
 *     var xf = t.dropWhile(function(n) { return n < 3; });
 *     t.into([], xf, [0,1,2,3,4,5]); // [3,4,5];
 */
/*@ dropWhile :: forall IN INTER OUT M . (pred: (IN)=>boolean) => {(xf: MTransformer<IN, INTER, OUT>) => DropWhile<M, IN, INTER, OUT> | true} */
function dropWhile<IN, INTER, OUT>(pred: (IN)=>boolean): (xf: Transformer<IN, INTER, OUT>) => DropWhile<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = pred;
    if(TRANSDUCERS_DEV && (typeof pred !== "function")) {
        throw new Error("dropWhile must be given a function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {DropWhile<M, IN, INTER, OUT> | true} */
        { return new DropWhile(ff, xf) };
    }
}

/*@ readonly NONE :: # */ 
var NONE = {};

class PartitionBy<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
public f: (z:IN) => any;
/*@ xf : MTransformer<MArray<IN>, INTER, OUT> */
public xf: Transformer<Array<IN>, INTER, OUT>;
/*@ a : MArray<IN> */
public a: Array<IN>;
/*@ pval : top */
public pval: any;
/*@ new(f:(IN) => top, xf:MTransformer<MArray<IN>, INTER, OUT>) => {PartitionBy<M, IN, INTER, OUT> | true} */
constructor(f: (z:IN) => any, xf: Transformer<Array<IN>, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
    this.a = [];
    this.pval = NONE;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (this:PartitionBy<Mutable,IN,INTER,OUT>, result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    if(this.a.length > 0) {
        result = this.xf.step(result.value, this.a);
        result.__transducers_reduced__ = 0;
        this.a = [];
    }
    return this.xf.result(result);
}
/*@ step : (this:PartitionBy<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    var pval = this.pval,
        val  = this.f(input);

    this.pval = val;

    // NOTE: we should probably allow someone to define
    // equality? - David
    if((pval === NONE) ||
       (pval === val)) {
        this.a.push(input);
        return new QQ(result, 0);
    } else {
        var ret = this.xf.step(result, this.a);
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
 *     var t = transducers;
 *     var xf = t.partitionBy(function(x) { return typeof x == "string"; });
 *     t.into([], xf, [0,1,"foo","bar",2,3,"bar","baz"]); // [[0,1],["foo","bar"],[2,3],["bar","baz"]];
 */
/*@ partitionBy :: forall IN INTER OUT M . (f: (IN)=>top) => {(xf: MTransformer<MArray<IN>, INTER, OUT>) => PartitionBy<M, IN, INTER, OUT> | true} */
function partitionBy<IN, INTER, OUT>(f: (IN)=>any): (xf: Transformer<Array<IN>, INTER, OUT>) => PartitionBy<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("partitionBy must be given an function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<MArray<IN>, INTER, OUT>) => {PartitionBy<M, IN, INTER, OUT> | true} */
        { return new PartitionBy(ff, xf) };
    }
}

class PartitionAll<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
public n: number;
/*@ xf : MTransformer<MArray<IN>, INTER, OUT> */
public xf: Transformer<Array<IN>, INTER, OUT>;
/*@ a : MArray<IN> */
public a: Array<IN>;
/*@ new(n:number, xf:MTransformer<MArray<IN>, INTER, OUT>) => {PartitionAll<M, IN, INTER, OUT> | true} */
constructor(n:number, xf:Transformer<Array<IN>, INTER, OUT>) {
    this.n = n;
    this.xf = xf;
    this.a = [];
}

init():INTER {
    return this.xf.init();
}
/*@ result : (this:PartitionAll<Mutable,IN,INTER,OUT>, result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    if(this.a.length > 0) {
        result = this.xf.step(result.value, this.a);
        result.__transducers_reduced__ = 0;
        this.a = [];
    }
    return this.xf.result(result);
}
/*@ step : (this:PartitionAll<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    this.a.push(input);
    if(this.n === this.a.length) {
        var a = this.a;
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
 *     var t = transducers;
 *     var xf = t.partitionAll(3);
 *     t.into([], xf, [0,1,2,3,4,5]); // [[0,1,2],[3,4,5]]
 */
/*@ partitionAll :: forall IN INTER OUT M . (n:number) => {(xf: MTransformer<MArray<IN>, INTER, OUT>) => PartitionAll<M, IN, INTER, OUT> | true} */
function partitionAll<IN, INTER, OUT>(n:number): (xf: Transformer<Array<IN>, INTER, OUT>) => PartitionAll<IN, INTER, OUT> {
    /*@ readonly nn :: # */ 
    var nn = n;
    if(TRANSDUCERS_DEV && (typeof n !== "number")) {
        throw new Error("partitionAll must be given a number");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<MArray<IN>, INTER, OUT>) => {PartitionAll<M, IN, INTER, OUT> | true} */
        { return new PartitionAll(nn, xf) };
    }
}

class Keep<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
public f: (z:IN) => any;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(f:(IN) => top, xf:MTransformer<IN, INTER, OUT>) => {Keep<M, IN, INTER, OUT> | true} */
constructor(f: (z:IN) => any, xf: Transformer<IN, INTER, OUT>) {
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    var v = this.f(input);
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
 *     var t = transducers;
 *     var xf = t.keep(function(x) { if(typeof x == "string") return "cool"; });
 *     t.into([], xf, [0,1,"foo",3,4,"bar"]); // ["foo","bar"]
 */
/*@ keep :: forall IN INTER OUT M . (f: (IN)=>top) => {(xf: MTransformer<IN, INTER, OUT>) => Keep<M, IN, INTER, OUT> | true} */
function keep<IN, INTER, OUT>(f: (IN)=>any): (xf: Transformer<IN, INTER, OUT>) => Keep<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("keep must be given a function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {Keep<M, IN, INTER, OUT> | true} */
        { return new Keep(ff, xf) };
    }
}

class KeepIndexed<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ i : number */
public i: number;
public f: (idx:number, z:IN) => any;
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ new(f:(idx:number, z:IN) => top, xf:MTransformer<IN, INTER, OUT>) => {KeepIndexed<M, IN, INTER, OUT> | true} */
constructor(f: (idx:number, z:IN) => any, xf: Transformer<IN, INTER, OUT>) {
    this.i = -1;
    this.f = f;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (this:KeepIndexed<Mutable,IN,INTER,OUT>, result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    this.i++;
    var v:any = this.f(this.i, input);
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
 *     var t = transducers;
 *     var xf = t.keepIndexed(function(i, x) { if(typeof x == "string") return "cool"; });
 *     t.into([], xf, [0,1,"foo",3,4,"bar"]); // ["foo","bar"]
 */
/*@ keepIndexed :: forall IN INTER OUT M . (f: (idx:number, z:IN)=>top) => {(xf: MTransformer<IN, INTER, OUT>) => KeepIndexed<M, IN, INTER, OUT> | true} */
function keepIndexed<IN, INTER, OUT>(f: (idx:number, z:IN)=>any): (xf: Transformer<IN, INTER, OUT>) => KeepIndexed<IN, INTER, OUT> {
    /*@ readonly ff :: # */ 
    var ff = f;
    if(TRANSDUCERS_DEV && (typeof f !== "function")) {
        throw new Error("keepIndexed must be given a function");
    } else {
        return function(xf) 
        /*@ <anonymous> forall M . (MTransformer<IN, INTER, OUT>) => {KeepIndexed<M, IN, INTER, OUT> | true} */
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
/*@ preservingReduced :: forall IN INTER OUT . (xf: MTransformer<IN, INTER, OUT>) => {MTransformer<IN, INTER, MQQ<INTER>> | true} */
function preservingReduced<IN, INTER, OUT>(xf: Transformer<IN, INTER, OUT>) {
    return new PreservingReduced(xf);
}

/*@ class PreservingReduced<M, IN, INTER> implements Transformer<M, IN, INTER, MQQ<INTER>> */
class PreservingReduced<IN, INTER> implements Transformer<IN, INTER, QQ<INTER>> {
/*@ xf : TruncatedTransformer<Mutable, IN, INTER> */
public xf: TruncatedTransformer<IN, INTER>;
/*@ new(xf:TruncatedTransformer<Mutable, IN, INTER>) => {PreservingReduced<M, IN, INTER> | true} */
constructor(xf: TruncatedTransformer<IN, INTER>) {
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {MQQ<INTER> | true} */
result(result:QQ<INTER>):QQ<INTER> {
    return result;
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, input:IN):QQ<INTER> {
    var ret = this.xf.step(result, input);
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
/*@ cat :: forall IN INTER OUT . (xf: MTransformer<IN, INTER, OUT>) => {MTransformer<IArray<IN>, INTER, OUT> | true} */
function cat<IN, INTER, OUT>(xf: Transformer<IN, INTER, OUT>) {
    return new Cat(xf);
}

/*@ class Cat<M, IN, INTER, OUT> implements Transformer<M, IArray<IN>, INTER, OUT> */
class Cat<IN, INTER, OUT> implements Transformer<Array<IN>, INTER, OUT> {
/*@ xf : MTransformer<IN, INTER, OUT> */
public xf: Transformer<IN, INTER, OUT>;
/*@ rxf : MTransformer<IN, INTER, MQQ<INTER>> */
public rxf: Transformer<IN, INTER, QQ<INTER>>;
/*@ new(xf:MTransformer<IN, INTER, OUT>) => {Cat<M, IN, INTER, OUT> | true} */
constructor(xf: Transformer<IN, INTER, OUT>) {
    this.xf = xf;
    this.rxf = preservingReduced(xf);
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.xf.result(result);
}
/*@ step : (result:INTER, input:IArray<IN>) : {MQQ<INTER> | true} */
step(result:INTER, input:Array<IN>):QQ<INTER> {
    return reduce(this.rxf, result, input);
}
}

/**
 * A mapping concatenating transformer
 * @method transducers.mapcat
 * @param {Function} f the mapping function
 * @return {Transducer} a mapping concatenating transducer
 * @example
 *     var t = transducers;
 *     var reverse = function(arr) { var arr = Array.prototype.slice.call(arr, 0); arr.reverse(); return arr; }
 *     var xf = t.mapcat(reverse);
 *     t.into([], xf, [[3,2,1],[6,5,4]]); // [1,2,3,4,5,6]
 */
/*@ mapcat :: forall IN INTER OUT S M . (f: (z:S)=>IArray<IN>) => {(xf: MTransformer<IN, INTER, OUT>) => Map<M, S, INTER, OUT, IArray<IN>> | true} */
function mapcat<IN, INTER, OUT, S>(f: (z:S)=>IN[]) {
    /*@ readonly ff :: # */ 
    var ff = f;
    return function(xf: Transformer<IN, INTER, OUT>) 
    /*@ <anonymous> forall M . (xf: MTransformer<IN, INTER, OUT>) => {Map<M, S, INTER, OUT, IArray<IN>> | true} */
    {
        return map(ff)(cat(xf))
    }
}

/*@ stringReduce :: forall INTER OUT . (xf:MTransformer<string, INTER, OUT>, init:INTER, str:string) => {OUT | true} */
function stringReduce<INTER, OUT>(xf:Transformer<string, INTER, OUT>, init:INTER, str:string) {
    var acc = init;
    var wrappedAcc = new QQ(acc, 0);
    var shouldBreak = false;
    for(var i = 0; i < str.length; i++) {
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

/*@ arrayReduce :: forall IN INTER OUT . (xf:MTransformer<IN, INTER, OUT>, init:INTER, array:IArray<IN>) => {OUT | true} */
function arrayReduce<IN, INTER, OUT>(xf:Transformer<IN, INTER, OUT>, init:INTER, array:IN[]) {
    var acc = init;
    var wrappedAcc = new QQ(acc, 0);
    var shouldBreak = false;
    for(var i = 0; i < array.length && !shouldBreak; i++) {
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

/*@ objectReduce :: forall M INTER OUT . (xf:MTransformer<Entry<M>, INTER, OUT>, init:INTER, ob:EmptyObject<Immutable>) => {OUT | true} */
function objectReduce<INTER, OUT>(xf:Transformer<{}, INTER, OUT>, init:INTER, ob:{[key:string]:any}) {
    var acc = init;
    var wrappedAcc = new QQ(acc, 0);
    var shouldBreak = false;
    for(var p in ob) {
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

/*@ iterableReduce :: /\ forall INTER OUT . (xf:MTransformer<top, INTER, OUT>, init:INTER, iter:ObjIter) => {OUT | true} */
function iterableReduce(xf, init, iter) {
    var xiter;
    if("ATATiterator" in iter) {
        xiter = (<Iterable>iter).ATATiterator();
    } else {
        xiter = <Iterator>iter;
    }

    var acc = init;
    var wrappedAcc = new QQ(acc, 0);    
    var shouldBreak = false;
    var step = xiter.next();    
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
/*@ reduce :: /\ forall IN INTER OUT   . (xf: MTransformer<IN, INTER, OUT>,          init:INTER, coll:IArray<IN>) => {OUT | true}
              /\ forall    INTER OUT   . (xf: MTransformer<string, INTER, OUT>,      init:INTER, coll:string)     => {OUT | true}
              /\ forall    INTER OUT M . (xf: MTransformer<Entry<M>, INTER, OUT>,    init:INTER, coll:ObjNoIter)  => {OUT | true}
              /\ forall    INTER OUT   . (xf: MTransformer<top, INTER, OUT>,         init:INTER, coll:ObjIter)    => {OUT | true}
              /\ forall IN       OUT   . (stepFn: (result:OUT, input:IN)=>OUT,       init:OUT,   coll:IArray<IN>) => {OUT | true}
              /\ forall          OUT   . (stepFn: (result:OUT, input:string)=>OUT,   init:OUT,   coll:string)     => {OUT | true}
              /\ forall          OUT M . (stepFn: (result:OUT, input:Entry<M>)=>OUT, init:OUT,   coll:ObjNoIter)  => {OUT | true}
              /\ forall          OUT   . (stepFn: (result:OUT, input:top)=>OUT,      init:OUT,   coll:ObjIter)    => {OUT | true} */
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
 *     var t = transducers;
 *     var inc = function(n) { return n+1; };
 *     var isEven = function(n) { return n % 2 == 0; };
 *     var apush = function(arr,x) { arr.push(x); return arr; };
 *     var xf = t.comp(t.map(inc),t.filter(isEven));
 *     t.transduce(xf, apush, [], [1,2,3,4]); // [2,4]
 */
/*@ transduce :: /\ forall A B C X Y Z   . (xf: MTransducer<A, B, C, X,        Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:IArray<X>) => {Z | true}
                 /\ forall A B C   Y Z   . (xf: MTransducer<A, B, C, string,   Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:string)    => {Z | true}
                 /\ forall A B C   Y Z M . (xf: MTransducer<A, B, C, Entry<M>, Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:ObjNoIter) => {Z | true}
                 /\ forall A B C   Y Z   . (xf: MTransducer<A, B, C, top,      Y, Z>, f: MTransformer<A, B, C>,  init:Y, coll:ObjIter)   => {Z | true}
                 /\ forall A   C X Y Z   . (xf: MTransducer<A, C, C, X,        Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:IArray<X>) => {Z | true}
                 /\ forall A   C   Y Z   . (xf: MTransducer<A, C, C, string,   Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:string)    => {Z | true}
                 /\ forall A   C   Y Z M . (xf: MTransducer<A, C, C, Entry<M>, Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:ObjNoIter) => {Z | true}
                 /\ forall A   C   Y Z   . (xf: MTransducer<A, C, C, top,      Y, Z>, f: (result:C, input:A)=>C, init:Y, coll:ObjIter)   => {Z | true} */
function transduce(xf:any, f:any, init:any, coll:any) {
    f = typeof f === "function" ? wrap(f) : f;
    xf = xf(f);
    return reduce(xf, init, coll);
}

// TODO: should be (string, string + number + boolean) => string
/*@ stringAppend :: (string, string) => {string | true} */
function stringAppend(s, x) {
    return s + x;
}

/*@ arrayPush :: forall T . (arr:MArray<T>, x:T) => {MArray<T> | true} */
function arrayPush<T>(arr:T[], x:T) {
    arr.push(x);
    return arr;
}

/*@ addEntry :: forall M T . (ob: MMap<T>, entry: Pair<M,string,T>) => { MMap<T> | true } */
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
 *     var t = transducers;
 *     var inc = function(n) { return n+1; };
 *     var isEven = function(n) { return n % 2 == 0; };
 *     var apush = function(arr,x) { arr.push(x); return arr; };
 *     var xf = t.comp(t.map(inc),t.filter(isEven));
 *     t.into([], xf, [1,2,3,4]); // [2,4]
 */
//TODO: when empty is a string, xf should actually be (MTransformer<string + number + boolean, string, string>) =>...
/*@ into :: /\ forall     Z     . (empty: string,    xf: MTransducer<string, string, string,             string,   string,    Z>, coll: string)    => {Z | true}
            /\ forall   X Z     . (empty: string,    xf: MTransducer<string, string, string,             X,        string,    Z>, coll: IArray<X>) => {Z | true}
            /\ forall     Z M   . (empty: string,    xf: MTransducer<string, string, string,             Entry<M>, string,    Z>, coll: ObjNoIter) => {Z | true}
            /\ forall     Z     . (empty: string,    xf: MTransducer<string, string, string,             top,      string,    Z>, coll: ObjIter)   => {Z | true}
            /\ forall T   Z     . (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            string,   MArray<T>, Z>, coll: string)    => {Z | true}
            /\ forall T X Z     . (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            X,        MArray<T>, Z>, coll: IArray<X>) => {Z | true}
            /\ forall T   Z M   . (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            Entry<M>, MArray<T>, Z>, coll: ObjNoIter) => {Z | true}
            /\ forall T   Z     . (empty: MArray<T>, xf: MTransducer<T, MArray<T>, MArray<T>,            top,      MArray<T>, Z>, coll: ObjIter)   => {Z | true}
            /\ forall T   Z N   . (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, string,   MMap<T>,   Z>, coll: string)    => {Z | true}
            /\ forall T X Z N   . (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, X,        MMap<T>,   Z>, coll: IArray<X>) => {Z | true}
            /\ forall T   Z N M . (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, Entry<M>, MMap<T>,   Z>, coll: ObjNoIter) => {Z | true}
            /\ forall T   Z N   . (empty: MMap<T>,   xf: MTransducer<Pair<N,string,T>, MMap<T>, MMap<T>, top,      MMap<T>,   Z>, coll: ObjIter)   => {Z | true} */
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

class Completing<IN, INTER, OUT> implements Transformer<IN, INTER, OUT> {
/*@ cf : (z:MQQ<INTER>) => OUT */
public cf: (z:QQ<INTER>) => OUT;
/*@ xf : TruncatedTransformer<Mutable, IN, INTER> */
public xf: TruncatedTransformer<IN, INTER>;
/*@ new(cf:(z:MQQ<INTER>) => OUT, xf:TruncatedTransformer<Mutable, IN, INTER>) => {Completing<M, IN, INTER, OUT> | true} */
constructor(cf: (z:QQ<INTER>) => OUT, xf: TruncatedTransformer<IN, INTER>) {
    this.cf = cf;
    this.xf = xf;
}

init():INTER {
    return this.xf.init();
}
/*@ result : (result:MQQ<INTER>) : {OUT | true} */
result(result:QQ<INTER>):OUT {
    return this.cf(result);
}
/*@ step : (result:INTER, input:IN) : {MQQ<INTER> | true} */
step(result:INTER, step:IN):QQ<INTER> {
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
/*@ completing :: /\ forall IN INTER OUT M T . (xf: MTransformer<IN, INTER, T>,      cf: (MQQ<INTER>) => OUT) => {Completing<M, IN, INTER, OUT       > | true}
                  /\ forall IN INTER OUT M   . (xf: (result:INTER, input:IN)=>INTER, cf: (MQQ<INTER>) => OUT) => {Completing<M, IN, INTER, OUT       > | true}
                  /\ forall IN INTER OUT M T . (xf: MTransformer<IN, INTER, T>)                               => {Completing<M, IN, INTER, MQQ<INTER>> | true}
                  /\ forall IN INTER OUT M   . (xf: (result:INTER, input:IN)=>INTER)                          => {Completing<M, IN, INTER, MQQ<INTER>> | true} */
function completing<IN, INTER, OUT>(xf: any, cf?: (z:QQ<INTER>) => OUT):any {
    var wxf:TruncatedTransformer<IN, INTER> = typeof xf === "function" ? wrap(xf) : xf;
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
 *     var t = transducers;
 *     var arr = [0,1,2,3,4,5],
 *     var apush = function(arr, x) { arr.push(x); return arr; },
 *     var xf = t.comp(t.map(inc),t.filter(isEven));
 *     arr.reduce(t.toFn(xf, apush), []); // [2,4,6]
 */
/*@ toFn :: /\ forall A B C X Y T . (xf: MTransducer<A, B, C, X, Y, T>, builder: MTransformer<A, B, C>)  => {(result:Y, input:X) => MQQ<Y> | true}
            /\ forall A   C X Y T . (xf: MTransducer<A, C, C, X, Y, T>, builder: (result:C, input:A)=>C) => {(result:Y, input:X) => MQQ<Y> | true} */
function toFn<IN, INTER, OUT>(xf, builder) {
    if(typeof builder === "function") {
        return xf(wrap(builder)).step;//TODO: see below
    }
    var rxf = xf(builder);
    return rxf.step//TODO: .bind(rxf);
}

// =============================================================================
// Utilities

/**
 * A transformer which simply returns the first input.
 * @method transducers.first
 * @return {Transducer} a transducer transformer
 */
var first:Wrap<any, any> = generalWrap(function(result:any, input:any) 
    /*@ <anonymous> (result:top, input:top) => {MQQ<top> | true} */
    { return new QQ(input, 1); }
);

// =============================================================================
// Exporting
// NOTICE: this section was removed as irrelevant to the JS->RS port

}}} // end of module com.cognitect.transducers
