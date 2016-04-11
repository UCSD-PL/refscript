/// <reference path="include/d3.d.ts" />

//TODO: move to prelude? why does the annotation make rsc stall?
/* Infinity :: {number | v = numeric_positive_infinity} */
declare let Infinity:number;

/*@ d3_range :: /\ (number, number, {number | v != 0}) =>   MArray<number>
                /\ (number, number                   ) => { MArray<number> | 0 < 1 }
                /\ (number                           ) => { MArray<number> | 0 < 1 } */
function d3_range(start:number, arg_stop?:number, arg_step?:number): number[] {
  /*@ local loc_start :: number + undefined */
  let loc_start = start;
  /*@ local loc_stop :: number + undefined */
  let loc_stop = arg_stop;
  /*@ local loc_step :: number + undefined */
  let loc_step = arg_step;
  /*@ local ONE :: number + undefined */
  let ONE = 1;

  if (arguments.length < 3) {
    loc_step = ONE;
    if (arguments.length < 2) {
      loc_stop = loc_start;
      start = 0;
    }
  }

  let stop = <number>loc_stop;
  let step = <number>loc_step;

  if ((stop - start) / step === Infinity) throw new Error("infinite range");
  let range:number[] = [];
  let k:number = d3_range_integerScale(Math.abs(step));
  let i = 0;
  start *= k; stop *= k; step *= k;
  let j = start;
  if (step < 0) while (j > stop) { range.push(j / k); i++; j = start + step * i; }
  else          while (j < stop) { range.push(j / k); i++; j = start + step * i; }
  return range;
};

function d3_range_integerScale(x:number):number {
  let k = 1;
  while (x * k % 1) k *= 10;
  return k;
}

// d3.range = d3_range;
