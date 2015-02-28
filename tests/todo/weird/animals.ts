// TODO: Allow WIDTH subtyping to function-parameters

class Animal {
  
  /*@ name : string */
  public name:string;

  /*@ new (string) => void */
  constructor(name:string) { 
    //this.name = name;
  }

  /*@ move : (number): void */
  move(meters:number) {
    alert(this.name + " moved " + meters + "m.");
  }

}

/*@ class Snake extends #Animal */
class Snake extends Animal {
  /*@ move : (): void */
  move() {
    alert("Slithering...");
    super.move(5);
  }
}


/*@ class Snake extends #Animal */
class Horse extends Animal {
  /*@ move : (meters: number): void */
  move(meters:number) {
    alert("Galloping...");
    super.move(45);
  }
}

var sam = new Snake("Sammy the Python")

/*@ tom :: #Animal */
var tom: Animal = new Horse("Tommy the Palomino")

sam.move();

//XXX: Weirdness in parameters
tom.move(34)
