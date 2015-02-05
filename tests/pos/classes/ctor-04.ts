
class AA {
  
  /*@ k : [Immutable] number */
  public k: number;
  
  /*@ l : [Immutable] number */
  public l: number;
  
  /*@ new () => { AA<M> | Map_select(v,"k") ~~ Map_select(v,"l") } */
  constructor() {
    var n = random();
    this.k = n;
    this.l = n;
  }
    
}

var n = new AA();
