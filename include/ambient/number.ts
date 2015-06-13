

/**
 *  NUMBER 
 * 
 *  TODO: 
 *
 *    - NaN =/= NaN
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

/**
 * 
 *  All numberic values inherit from this type
 *
 */
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
    /*@ floor : (x: number) : {number | x - 1 < v && v <= x} */
    floor(x: number): number;
    log(x: number): number;
    /*@ max : (a:number, b:number) : {number | v = if (a < b) then b else a} */
    max(a: number, b: number): number;
    // max(...values: number[]): number;
    /*@ min : (a:number, b:number) : {number | v = if (a < b) then a else b} */
    min(a: number, b:number): number;
    // min(...values: number[]): number;
    pow(x: number, y: number): number;
    random(): number;
    round(x: number): number;
    sin(x: number): number;
    /*@ sqrt : (x:{number | v >= 0}) : {number | v = 0 <=> x = 0} */
    sqrt(x: number): number;
    tan(x: number): number;
}

declare var Math: Math;


