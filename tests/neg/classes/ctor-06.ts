

class AA { 
  /*@ a : [Immutable] { string | v = "OLD" } */
  public a = "OLD"; 

  /*@ new () => { AA<M> | true } */
  constructor () {}
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  public b = 0;
    
  /*@ new () => { BB<M> | true } */
  constructor() {
    super();
    this.a = "NEW";
  }
    
}

var n = new BB();
