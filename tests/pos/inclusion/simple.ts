
/*@ measure haskey :: forall A . (string, A) => bool */

/*@ m :: [Immutable] { [k: string]: number }*/
declare var m: { [k: string]: number };

/*@ arr :: { IArray<{ v: string | haskey v m }> | len v = 10 } */
declare var arr: number[];

var r = arr[0];
