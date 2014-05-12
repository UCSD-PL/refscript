

/*@ leng :: (a: #Array[#Immutable,number]) => { number | (v = (len a)) } */
function leng(a: number[]) {

  return a.length;

}
