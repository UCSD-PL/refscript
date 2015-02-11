

class AA { 
  /*@ a : [Immutable] string */
  public a = "OLD"; 

  constructor () {}
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  /*@ a : [Immutable] { string | v = "NEW" } */
  public a: string;

  public b = 0;
    
  /*@ new () => BB<M> */
  constructor() {
    super();
    this.a = "NEW";
  }
    
}

var n = new BB();
