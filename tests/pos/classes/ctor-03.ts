

class AA { 
  /*@ a : [Immutable] string */
  public a = "OLD"; 
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  public b = 0;
    
  /*@ new () => { v: BB<M> | Map_select(v,"a") ~~ "NEW" } */
  constructor() {
    super();
    this.a = "NEW";
  }
    
}

var n = new BB();
