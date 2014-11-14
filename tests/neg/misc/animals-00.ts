

/*@ predicate Inst(X, Key, Val, Type) = ((keyVal(X, Key) = Val) => instanceof (X, Type)) */

/*@ predicate InstHorse(V) = Inst(V,"kind","horse","Horse") */
/*@ predicate InstSnake(V) = Inst(V,"kind","snake","Snake") */
/*@ predicate InstTiger(V) = Inst(V,"kind","tiger","Tiger") */

/*@ alias AnimalK = { v: Animal<Immutable> | InstHorse(v) && InstSnake(v) && InstTiger(v) } */

class Animal {
  public kind = "";
}

class Horse extends Animal { 
  public kind = "horse";
  public gallop() {} 
}

class Snake extends Animal { 
  public kind = "snake";
  public sneak() {}
}

class Tiger extends Animal { 
  public kind = "tiger";
  public run() {}
}

/*@ move :: (a: AnimalK) => { void | true } */
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
