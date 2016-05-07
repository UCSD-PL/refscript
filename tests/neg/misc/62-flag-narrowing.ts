
/*@ predicate non_zero(b) = (b /= lit "#x00000000" (BitVec (Size32))) */
/*@ predicate mask_symbolflags_transient(v) = (non_zero(bvand v (lit "#x02000000" (BitVec (Size32))))) */

type ISymbol      = Symbol<Immutable>
type ISymbolLinks = SymbolLinks<Immutable>

export enum SymbolFlags {

    Merged    = 0x01000000,
    Transient = 0x02000000
}

export interface Symbol<M extends ReadOnly> {
    /*@ (Immutable) flags: { v: bitvector32 | mask_symbolflags_transient v <=>  extends_interface this "TransientSymbol" } */
    flags: SymbolFlags;
}

export interface SymbolLinks<M extends ReadOnly> {

}

export interface TransientSymbol<M extends ReadOnly> extends Symbol<M>, SymbolLinks<M> { }

export let getSymbolLinks = function(symbol: ISymbol): ISymbolLinks {
    if (symbol.flags & SymbolFlags.Merged) {
        return <TransientSymbol<Immutable>>symbol;
    }
    throw new Error("")
}
