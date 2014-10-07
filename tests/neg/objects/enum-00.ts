
enum Operator {
  ADD,
  DIV,
  MUL,
  SUB
}

function compute(op: Operator, a: number, b: number) {
  if (op === Operator.SUB) 
    return a + b;
  else if (op === Operator.ADD) 
    return a - b;
  else if (op === Operator.MUL) 
    return a * b;
  else if (op === Operator.DIV)
    return a / b;
  else 
    throw new Error("impossible")
}


assert(compute(Operator.ADD, 1, 2) === 3);
assert(compute(Operator.SUB, 2, 1) === 1  );
