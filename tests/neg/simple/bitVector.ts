
/*@ predicate Field_BV_Set(X, F, I)          = bv_idx(keyVal(X, F), I) */

/*@ predicate Field_BV_Imp_Type (X, F, I, T) = (Field_BV_Set(X, F, I) => extends_interface(X, T)) */

// The number 0x02000000 == 0b_0010_0000_0000_0000_0000_0000_0000 --> a '1' in the 25-th position

/*@ predicate P_TransientSymbol(X)           = Field_BV_Imp_Type(X, "flags", 25, "TransientSymbol") */

/*@ alias ISymbolF                           = { v: Symbol<Immutable> | P_TransientSymbol(v)    } */


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

/*@ getSymbolLinks :: (symbol: ISymbolF) => { void | true } */
function getSymbolLinks(symbol: Symbol): void {
    if ((symbol.flags & SymbolFlags.Transient) === SymbolFlags.Merged) {
        var ts = <TransientSymbol>symbol;
    }
}

