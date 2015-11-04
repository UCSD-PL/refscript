
/*@ minIndex :: (aa: IArray<number>) => {number | true} */
function minIndex(aa){

  /*@ readonly a :: # */
  var a = aa;

  if (a.length <= 0) return -1;

  function body(acc: number, cur: number, i: number) {
      return cur < a[acc] ? i : acc;
  };

  return a.reduce(body, 0);
}
