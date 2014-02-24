
/*@ foo :: ({[number] | (len v) > 0 }) => { number | true } */
/* foo :: (number) => { number | true } */
function foo(a : number []) : number  {

  return a[0];

}
