class Animal {
  public kind = "";
  constructor() { }
}
class Horse extends Animal { 
  public kind = "horse";
  public gallop() {} 
  constructor() { super(); }
}
class Snake extends Animal { 
  public kind = "snake";
  public sneak() {} 
  constructor() { super(); }
}

/*@ move :: (a: Animal) => { void | 0 < 1} */
function move(a: Animal) {

  if (a.kind === "snake") {
    var h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "horse") {
    var s = <Snake>a;
    s.sneak();
  }

}

