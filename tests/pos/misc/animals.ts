

/*@ predicate Inst(X, Key, Val, Type) = ((keyVal(X, Key) = Val) => extends_class (X, Type)) */

/*@ predicate InstHorse(V) = Inst(V,"kind","horse","Horse") */
/*@ predicate InstSnake(V) = Inst(V,"kind","snake","Snake") */
/*@ predicate InstTiger(V) = Inst(V,"kind","tiger","Tiger") */

/*@ alias AnimalK = { v: Animal<Immutable> | InstHorse(v) && InstSnake(v) && InstTiger(v) } */

class Animal {
  public kind = "";
  constructor() {}
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

/*@ move :: (a: AnimalK) => { void | true } */
function move(a: Animal) {
  if (a.kind === "horse") {
    var h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "snake") {
    var s = <Snake>a;
    s.sneak();
  }
}
