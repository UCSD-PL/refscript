

class AA { 
  public a = "OLD"; 
}

/*@ class BB<M> extends AA<M> */
class BB extends AA {
  
  public b: number;
    
  /*@ new () => { v: BB<M> | Map_select(v,"a") ~~ "NEW" } */
  constructor() {
    super();
    this.a = "NEW";
    super();
  }
    
}

var n = new BB();
