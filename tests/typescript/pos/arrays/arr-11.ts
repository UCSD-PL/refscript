/*@ foo :: (x: { number | v > 122 }) => void */
function foo (x: number) {}

/*@ a :: #Array[#Immutable, number] */
var a : number[] = [1,2,3];
/*@ b :: { #Array[#Immutable, number] | (len v) = 4 } */
var b : number[] = [1,2,3,4];

assert(a.length + b.length == 7);
