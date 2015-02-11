
class AA { 
  /*@ x : [Immutable] number */
  public x = 0; 
  
  /*@ y : [Immutable] { number | v = x } */
  public y = 0;
    
  constructor() { }
    
}

var aa = new AA();

assert(aa.x === aa.y);

