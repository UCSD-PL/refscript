
/*@ bar :: (cat: number) => #Array[#Immutable, { number | v >= cat }]  */
function bar(cat : number) : number[] {
  return [cat, cat + 1, cat + 2];
}

