

class Animal {
  /*@ kind : [Immutable] { v: string | [ ((v = "") || (v = "horse") || (v = "snake") || (v = "tiger"));
                                         (v = "horse") => extends_class(this, "Horse");
                                         (v = "snake") => extends_class(this, "Snake");
                                         (v = "tiger") => extends_class(this, "Tiger")] } */
  public kind = "";
  constructor() {}
}

class Horse extends Animal { 
  public gallop() {} 
  constructor() { 
    super(); 
    this.kind = "horse";
  }
}

class Snake extends Animal { 
  public sneak() {}
  constructor() { 
    super(); 
    this.kind = "snake";
  }
}

class Tiger extends Animal { 
  public run() {}
  constructor() { 
    super(); 
    this.kind = "tiger";
  }
}

/*@ move :: (a: Animal<Immutable>) => { void | 0 < 1} */
function move(a: Animal) {
  var k = a.kind;
  if (a.kind === "horse") {
    var h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "tiger") {
    var s = <Snake>a;
    s.sneak();
  }
}
