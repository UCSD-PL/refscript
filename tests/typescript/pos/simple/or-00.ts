/* emp :: { #Array[#Immutable, number] | (len v) = 0 }  */
var emp = [];

/* single :: { #Array[#Immutable, number] | (len v) = 1 }  */
var single = [1];

var array = emp || single;

assert(array.length < 2);

