
/*@ bar :: (cat:number) => [{number | v >= cat}]  */
function bar(cat) {
  return [cat, cat + 1, cat + 2];
}

