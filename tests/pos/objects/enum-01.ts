
enum Operator {
  ADD,
  DIV,
  MUL,
  SUB
}


assert(Operator.ADD === 0);

assert(Operator[Operator.ADD] === "ADD");

assert(Operator[0] === "ADD") ;