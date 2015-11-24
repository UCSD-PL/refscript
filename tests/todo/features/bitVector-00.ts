

/*@ predicate IsField(x,f,t)    = keyVal(x,f) & t = t  */

/*@ predicate P_Symbol(X, B, N) = IsField(X, "flags", B) => extends_interface(X, N) */

/*@ alias ISymbolF              = { v: Symbol<Immutable> | P_Symbol(v, 0x02000000, "Transient") &&
                                                           P_Symbol(v, 0x01000000, "Merged")      } */

enum SymbolFlags {
    Import             = 0x00400000,  // Import
    Instantiated       = 0x00800000,  // Instantiated symbol
    Merged             = 0x01000000,  // Merged symbol (created during program binding)
    Transient          = 0x02000000,  // Transient symbol (created during type check)   
    Prototype          = 0x04000000,  // Symbol for the prototype property (without source code representation)
}

interface Symbol {
    flags: SymbolFlags;
    name: string; 
}

interface TransientSymbol extends Symbol { }

/*@ getSymbolLinks :: (symbol: ISymbolF) => { void | 0 < 1 } */
function getSymbolLinks(symbol: Symbol): void {

    if (symbol.flags & SymbolFlags.Transient) {

        var ts = <TransientSymbol>symbol;

    }

}

