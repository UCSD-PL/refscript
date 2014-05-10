
/*@ qualif Len(v:number) : (len v) = 3           */
/*@ qualif Len(v:number) : (len v) = 4           */

/*@ a :: #Array[#Immutable, number] */
var a : number[] = [1,2,3];
/*@ b :: #Array[#Immutable, number] */
var b : number[] = [1,2,3,4];

assert(a.length + b.length == 7);
