
class AA {
  
  /*@ k : [Immutable] number */
  public k: number;
  
  /*@ l : [Immutable] { number | v = this.k } */
  public l: number;
  
  constructor() {
    var n = random();
    this.k = n;
    this.l = n;
  }
    
}

var n = new AA();

assert(n.k === n.l);

