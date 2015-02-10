
// OBJECT LITERAL METHODS VS FUNCTION FIELDS 


interface NumberGenerator {
  generate: () => number;
}

/*@ foo :: () => {NumberGenerator<Immutable> | true} */
function foo() {

  return {

    generate: function() 
    /*@ <anonymous>  () => { number | true } */
    { return 3; }

  }

}
