/*@ DATA_SIZE :: number */
var DATA_SIZE = 3;

class Packet {
  /*@ a2 : {Array<Immutable, number> | (len v) = DATA_SIZE} */
  public a2;

  /*@ new() => {void | true} */
  constructor() {
    this.a2 = new Array(DATA_SIZE);
    DATA_SIZE++;
  }
}

