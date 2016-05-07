
enum Operator { ADD, SUB, DIV, MUL }

module A {


    /*@ compute :: (o: number,
                    a: number,
                    b: { v: number | ((o = 2) => (v != 0))}
                   ) =>
                    { number | [(o = 0) => (v = a + b);
                                (o = 1) => (v = a - b)]
                    } */
    function compute(op: Operator, a: number, b: number): number {
      if (op === Operator.ADD)
        return a + b;
      else if (op === Operator.SUB)
        return a - b;
      else if (op === Operator.MUL)
        return a * b;
      else if (op === Operator.DIV)
        return a / b;
      else
        throw new Error("");
    }

    let opa = Operator.ADD;

    assert(compute(Operator.ADD, 1, 2) === 3);
    assert(compute(Operator.SUB, 2, 1) === 1);

}
