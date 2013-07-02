/*@ type nlist  {  data : number, 
                   next : nlist  } */

/*@ type blist  {  data : boolean, 
                   next : blist  } */

/*@ map :: (nlist, (number) => boolean) => blist */
function map(x,f) {

  return { 
    data: f(x.data) , 
    next: map(x.next, f)
  };

}
