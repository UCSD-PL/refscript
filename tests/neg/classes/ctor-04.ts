
class AA {
  
  /*@ k : [Immutable] number */
  public k: number;
  
  /*@ l : [Immutable] { number | v = k } */
  public l: number;
  
  constructor() {
    this.k = random();
    this.l = random();
  }
    
}

var n = new AA();

assert(n.k === n.l);

