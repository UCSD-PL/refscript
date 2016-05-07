
// OBJECT LITERAL METHODS VS FUNCTION FIELDS

interface NumberGenerator<M extends ReadOnly> {
    generate: () => number;
}

export function foo(): NumberGenerator<Immutable> {
    return {
        generate: function() {
            return 3;
        }
    }
}
