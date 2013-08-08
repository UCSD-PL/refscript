
/*@ type nlist  list[number] */ 

/*@ foo :: (x: nlist + number) => nlist */
function foo(x) {
  return { data: 5, next: x   } ;
}
