

class AA { 
  /*@ k : [Immutable] string */
  public k = "VALUE";  

  constructor() {}

}

class BB extends AA {
  
  /*@ l : [Immutable] string */
  public l: string;
  
  /*@ new () => { BB<M> | Map_select(v,"k") ~~ Map_select(v,"l") } */
  constructor() {
    super();
    this.l = "VALUE";
  }
    
}

var n = new BB();
