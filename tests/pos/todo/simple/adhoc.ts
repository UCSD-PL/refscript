
// OBJECT LITERAL METHODS VS FUNCTION FIELDS

interface NumberGenerator<M extends ReadOnly> {
    generate: () => number;
}

export function foo(): NumberGenerator<Immutable> {
    return {
        generate: function() /*@ <anonymous>  () => { number | true } */ {
            return 3;
        }
    }
}
