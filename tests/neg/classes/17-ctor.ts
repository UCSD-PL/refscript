

class AA { 
  /*@ a : [Immutable] { string | v = "OLD" } */
  public a = "OLD"; 

  /*@ new () => { AA<M> | 0 < 1 } */
  constructor () {}
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  public b = 0;
    
  /*@ new () => { BB<M> | 0 < 1 } */
  constructor() {
    super();
    this.a = "NEW";
  }
    
}

var n = new BB();
