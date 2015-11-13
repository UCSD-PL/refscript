
// OBJECT LITERAL METHODS VS FUNCTION FIELDS 


interface NumberGenerator {
  generate: () => number;
}

/*@ foo :: () => {NumberGenerator<Immutable> | 0 < 1} */
function foo() {
  return {
    generate: function() /*@ <anonymous>  () => { number | true } */ { return 3; }
  }
}
