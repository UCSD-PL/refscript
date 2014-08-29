/*@ qualif CmpZ(v:number) : v = 0                   */
/*@ qualif Len(v:number) : (len v) = 3           */
/*@ qualif Len(v:number) : (len v) = 4           */

/*@ a :: #Array[#Immutable, number] */
var a  = [0,0,0];


/*@ b :: #Array[#Immutable, number] */
var b  = [0,0,0,0];

assert(a.length + b.length === 8);
