

//XXX: WORKS with this line - some qualifier scraping is missing...
/*  qualif Length(v:number): (len v) = 3 */

/*@ a :: #Array[#Immutable, number] */
var a : number[] = [1,2,3];
/*@ b :: { #Array[#Immutable, number] | (len v) = 4 } */
var b : number[] = [1,2,3,4];

assert(a.length + b.length == 7);
