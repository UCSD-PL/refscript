
/*@ alias ISymbol      = Symbol<Immutable>   */
/*@ alias IDeclaration = Declaration<Immutable>   */

enum NodeFlags { Export = 0x00000001 }

interface Symbol {
    /*@ declarations : IArray<IDeclaration> */
    declarations?: Declaration[];
}

interface Declaration extends Node { }

interface Node {
    /*@ flags : [Immutable] bitvector32 */
    flags: NodeFlags;
}

/*@ getDeclarationOfKind :: (symbol: ISymbol) => { void | 0 < 1 } */
function getDeclarationOfKind(symbol: Symbol): void {
    var declarations = symbol.declarations;
    for (var i = 0; i < declarations.length; i++) {
        var declaration = declarations[i];
    }
}
