
/*@ type nlist  list[number] */ 

/*@ foo :: () => nlist */
function foo() {
  return { data: 5, more_data: 6, next: null  } ;
}
