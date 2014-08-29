
/*@ bar :: (cat:number) => #Array[#Immutable,{number | v >= cat}]  */
function bar(cat) {
  return [cat, cat - 1, cat + 2];
}

