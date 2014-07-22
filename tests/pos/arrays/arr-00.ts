
/*@ qualif One(v:number)             : (len v) > 0    */

/*@ foo :: ( a: #Array[#Immutable,number] ) => number */

function foo( a : number []) : number  {

  return a[0];
}

foo([1]);
foo([1,2]);
foo([1,2,3]);
foo([1,2,3,4]);
