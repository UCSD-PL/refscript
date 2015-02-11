

class AA { 
  /*@ a : [Immutable] { string | v = "OLD" } */
  public a = "OLD"; 

  constructor () {}
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  public b = 0;
    
  /*@ new () => BB<M> */
  constructor() {
    super();
    this.a = "NEW";
  }
    
}

var n = new BB();
