
class AA { 

  /*@ x : [Mutable] number */
  public x = 0; 


  // Refinement CANNOT refer to mutable fields 
  
  /*@ y : [Immutable] { number | v = x } */
  public y = 0;
    
  constructor() { }
    
}

var aa = new AA();

aa.x = 1;

assert(aa.x === aa.y);

