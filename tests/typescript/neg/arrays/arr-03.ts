
/*@ indirectIndex :: (
    a: #Array[#Immutable,number], 
    b: #Array[#Immutable, { number | ((0 <= v) && (v <  (len a))) }], 
    i:   { number | ((0 <= v) && (v <= (len b))) }
  ) => number */
function indirectIndex(a, b, i) {

  return a[ b[i] ];

}
