

/*@ predicate Inst(X,K,V,T) = (keyVal(X, K) = V => instanceof (X, T)) */

/*@ predicate InstHorse(V)  = Inst(V, "kind", "horse", "Horse") */
/*@ predicate InstSnake(V)  = Inst(V, "kind", "snake", "Snake") */
/*@ predicate InstTiger(V)  = Inst(V, "kind", "tiger", "Tiger") */

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
  public sneak() {} }

/*@ move :: (a: AnimalK) => void */
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