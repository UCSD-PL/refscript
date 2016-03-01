/// <reference path="libs/types.ts"/>
/// <reference path="libs/core.defs.ts"/>

/*@ type nat = { number | v >= 0 } */


module checker_ts {

    /*@ nextSymbolId :: posint */
    let nextSymbolId = 1;
    /*@ nextNodeId :: posint */
    let nextNodeId = 1;
    /*@ nextMergeId :: posint */
    let nextMergeId = 1;

    /*@ getDeclarationOfKind :: (symbol: ISymbol, kind: ts.SyntaxKind) => { ts.Declaration<Immutable> | offset(v,"kind") = kind } + undefined */
    export function getDeclarationOfKind(symbol: ISymbol, kind: ts.SyntaxKind): IDeclaration {
        let declarations = symbol.declarations;
        for (let i = 0; i < declarations.length; i++) {
            let declaration = declarations[i];
            if (declaration.kind === kind) {
                return declaration;
            }
        }

        return undefined;
    }

    /// fullTypeCheck denotes if this instance of the typechecker will be used to get semantic diagnostics.
    /// If fullTypeCheck === true,  then the typechecker should do every possible check to produce all errors
    /// If fullTypeCheck === false, the typechecker can take shortcuts and skip checks that only produce errors.
    /// NOTE: checks that somehow affect decisions being made during typechecking should be executed in both cases.

    declare let program: ts.Program<Immutable>;

    declare let objectAllocator: cts.ObjectAllocator<Immutable>;

    let Symbol = objectAllocator.getSymbolConstructor();
    let Type = objectAllocator.getTypeConstructor();
    let Signature = objectAllocator.getSignatureConstructor();

    /*@ typeCount :: nat */
    let typeCount = 0;

    /*@ emptyArray :: <T>() => { IArray<T> | len v = 0 }  */
    let emptyArray = function(): any[] { return []; };

    let emptySymbols: ts.SymbolTable<Immutable> = {};

    let compilerOptions = program.getCompilerOptions();


    /*@ readonly */
    let checker: ts.TypeChecker<Immutable> = {
        //getProgram: () => program,
        //getDiagnostics: getDiagnostics,
        //getGlobalDiagnostics: getGlobalDiagnostics,
        //getNodeCount: () => sum(program.getSourceFiles(), "nodeCount"),
        //getIdentifierCount: () => sum(program.getSourceFiles(), "identifierCount"),
        //getSymbolCount: () => sum(program.getSourceFiles(), "symbolCount"),
        //getTypeCount: () => typeCount,
        //checkProgram: checkProgram,
        //emitFiles: invokeEmitter,
        //getParentOfSymbol: getParentOfSymbol,
        //getTypeOfSymbol: getTypeOfSymbol,
        //getPropertiesOfType: getPropertiesOfType,
        //getPropertyOfType: getPropertyOfType,
        //getSignaturesOfType: getSignaturesOfType,
        //getIndexTypeOfType: getIndexTypeOfType,
        //getReturnTypeOfSignature: getReturnTypeOfSignature,
        //getSymbolsInScope: getSymbolsInScope,
        //getSymbolInfo: getSymbolInfo,
        //getTypeOfNode: getTypeOfNode,
        //getApparentType: getApparentType,
        //typeToString: typeToString,
        //symbolToString: symbolToString,
        //getAugmentedPropertiesOfApparentType: getAugmentedPropertiesOfApparentType,
        //getRootSymbol: getRootSymbol,
        //getContextualType: getContextualType
    };


    /*@ global */ let mergedSymbols: MArray<ISymbol> = [];
    /*@ global */ let symbolLinks: MArray<ISymbolLinks> = [];
    /*@ global */ let nodeLinks: MArray<INodeLinks> = [];

    /*@ createSymbol :: <M extends ReadOnly>(flags: bitvector32, name: string) => ts.Symbol<M> */
    /*@ createSymbol :: <M extends ReadOnly>(flags: number     , name: string) => ts.Symbol<M> */
    declare function createSymbol<M extends ReadOnly>(flags: ts.SymbolFlags, name: string): ts.Symbol<M>

    /*@ getExcludedSymbolFlags :: (flags: ts.SymbolFlags) => bitvector32 */
    export function getExcludedSymbolFlags(flags: ts.SymbolFlags): ts.SymbolFlags {
        let result = 0x00000000; // 0;
        //TODO: undefined below
        //if (flags & SymbolFlags.Variable)      result = result | SymbolFlags.VariableExcludes;
        //if (flags & SymbolFlags.Property)      result = result | SymbolFlags.PropertyExcludes;
        //if (flags & SymbolFlags.EnumMember)    result = result | SymbolFlags.EnumMemberExcludes;
        //if (flags & SymbolFlags.Function)      result = result | SymbolFlags.FunctionExcludes;
        //if (flags & SymbolFlags.Class)         result = result | SymbolFlags.ClassExcludes;
        //if (flags & SymbolFlags.Interface)     result = result | SymbolFlags.InterfaceExcludes;
        //if (flags & SymbolFlags.Enum)          result = result | SymbolFlags.EnumExcludes;
        //if (flags & SymbolFlags.ValueModule)   result = result | SymbolFlags.ValueModuleExcludes;
        //if (flags & SymbolFlags.Method)        result = result | SymbolFlags.MethodExcludes;
        //if (flags & SymbolFlags.GetAccessor)   result = result | SymbolFlags.GetAccessorExcludes;
        //if (flags & SymbolFlags.SetAccessor)   result = result | SymbolFlags.SetAccessorExcludes;
        //if (flags & SymbolFlags.TypeParameter) result = result | SymbolFlags.TypeParameterExcludes;
        //if (flags & SymbolFlags.Import)        result = result | SymbolFlags.ImportExcludes;
        return result ;
    }

    export let recordMergedSymbol = function(target: ISymbol, source: ISymbol): void {
        if (!source.mergeId) source.mergeId = nextMergeId++;
        mergedSymbols[source.mergeId] = target;
    }

    export let getSymbolLinks = function(symbol: ISymbol): ISymbolLinks {
        if (symbol.flags & ts.SymbolFlags.Transient) {
            return <ts.TransientSymbol<Immutable>>symbol;
        }
        if (!symbol.id) symbol.id = nextSymbolId++;
        let s = symbolLinks[symbol.id];
        if(s) {
            return s;
        }
        else {
            let o: ISymbolLinks = {}; symbolLinks[symbol.id] = o; return o;
        }
    }

    export let getNodeLinks = function(node: INode): INodeLinks {
        let node_id = node.id;
        if (!node_id) {
            node_id = nextNodeId++;
            node.id = node_id;
            /*@ local node_id_0 :: number + undefined */
            let node_id_0 = node_id;
            node_id = node_id_0;
        }
        let n = nodeLinks[<number>node_id];
        if(n) { return n; }
        else { let o: INodeLinks = {}; nodeLinks[<number>node_id] = o; return o; }
    }


    /*@ getAncestor :: (node: INode + undefined, kind: ts.SyntaxKind) => undefined + { INode | offset v "kind" = kind } */
    export declare let getAncestor: (node: INode, kind: ts.SyntaxKind) => INode;
    // export let getAncestor = function(node: INode, kind: ts.SyntaxKind): INode {
    //     if (kind === ts.SyntaxKind.ClassDeclaration) {
    //         while (typeof node !== "undefined") {
    //             if (node.kind === ts.SyntaxKind.ClassDeclaration) {
    //                 //return <ClassDeclaration>node;
    //                 return <INode>node;
    //             }
    //             else if (kind === ts.SyntaxKind.EnumDeclaration      ||
    //                      kind === ts.SyntaxKind.InterfaceDeclaration ||
    //                      kind === ts.SyntaxKind.ModuleDeclaration    ||
    //                      kind === ts.SyntaxKind.ImportDeclaration) {
    //                 // early exit cases - declarations cannot be nested in classes
    //                 return undefined;
    //             }
    //             else {
    //                 node = node.parent;
    //             }
    //         }
    //     }
    //     else {
    //         while (node) {
    //             if (node.kind === kind) {
    //                 return <INode>node;
    //             }
    //             else {
    //                 node = node.parent;
    //             }
    //         }
    //     }
    //
    //     return undefined;
    // }

    /*@ getSourceFile :: (node: INode + undefined) => undefined + ts.SourceFile<Immutable> */
    export let getSourceFile = function(node: INode): ts.SourceFile<Immutable> {
        let ancestor = getAncestor(node, ts.SyntaxKind.SourceFile);
        if (ancestor) {
            return <ts.SourceFile<Immutable>> (<ts.Node<Immutable>>ancestor);
        } else {
            return undefined;
        }
    }


    /*@ createType :: (flags: bitvector32) => { ts.Type<Unique> | type_flags flags v } */
    let createType = function(flags: ts.TypeFlags): ts.Type<Unique> {
        /*@ result :: ts.Type<Unique> */
        let result = cts.newType(checker, flags);
        result.id = typeCount++;
        return result;
    }

    /*@ createObjectType :: ( kind: { bitvector32 | mask_typeflags_anonymous(v) || mask_typeflags_reference(v) }
                               , symbol: ISymbol + undefined
                           ) => { ts.ObjectType<Unique> | type_flags(kind,v) } */
    let createObjectType = function<M extends ReadOnly>(kind: ts.TypeFlags, symbol?: ISymbol): ts.ObjectType<M> {
        let type = <ts.ObjectType<Unique>>createType(kind);
        // TODO
        // type.symbol = symbol;
        return type;
    }



}
