/// <reference path="types.ts"/>
/// <reference path="core.defs.ts"/>
//  <reference path="parser.ts"/>
//  <reference path="scanner.ts"/>
//  <reference path="binder.ts"/>
//  <reference path="emitter.ts"/>

module ts {

    /*@ alias nat = { number | v > 0 } */
    /*@ nextSymbolId :: nat */
    var nextSymbolId = 1;
    /*@ nextNodeId :: nat */
    var nextNodeId = 1;
    /*@ nextMergeId :: nat */
    var nextMergeId = 1;

    /*@ getDeclarationOfKind :: (symbol: ISymbol, kind: SyntaxKind) => { Declaration<Immutable> | offset(v,"kind") = kind } + undefined */
    export function getDeclarationOfKind(symbol: Symbol, kind: SyntaxKind): Declaration {
        var declarations = symbol.declarations;
        for (var i = 0; i < declarations.length; i++) {
            var declaration = declarations[i];
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

        /*@ program :: Program<Immutable> */
        declare var program: Program;

        /*@ objectAllocator :: ObjectAllocator<Immutable> */
        declare var objectAllocator: ObjectAllocator;

        var Symbol = objectAllocator.getSymbolConstructor();
        var Type = objectAllocator.getTypeConstructor();
        var Signature = objectAllocator.getSignatureConstructor();

        /*@ typeCount :: { number | v >= 0 } */
        var typeCount = 0;

        /*@ emptyArray :: forall T . () => { IArray<T> | (len v) = 0 }  */
        function emptyArray(): any[] { return []; }; 

        var emptySymbols: SymbolTable = {};

        var compilerOptions = program.getCompilerOptions();

        /*@ readonly checker :: # */
        var checker: TypeChecker = {
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

        /*@ mergedSymbols :: MArray<ISymbol> */
        var mergedSymbols: Symbol[] = [];
        /*@ symbolLinks :: MArray<SymbolLinks<Immutable>> */
        var symbolLinks: SymbolLinks[] = [];
        /*@ nodeLinks :: MArray<NodeLinks<Immutable>> */
        var nodeLinks: NodeLinks[] = [];

        /*@ createSymbol :: /\ forall M . (flags: bitvector32, name: string) => Symbol<M>
                            /\ forall M . (flags: number     , name: string) => Symbol<M>
         */
        declare function createSymbol(flags: SymbolFlags, name: string): Symbol

        /*@ getExcludedSymbolFlags :: (flags: SymbolFlags) => { bitvector32 | true } */
        function getExcludedSymbolFlags(flags: SymbolFlags): SymbolFlags {
            var result = 0x00000000; // 0;
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

        /*@ recordMergedSymbol :: (target: ISymbol, source: ISymbol) => { void | true } */
        function recordMergedSymbol(target: Symbol, source: Symbol) {
            if (!source.mergeId) source.mergeId = nextMergeId++;
            mergedSymbols[source.mergeId] = target;
        }

        /*@ getSymbolLinks :: (symbol: ISymbol) => { SymbolLinks<Immutable> | true } */
        function getSymbolLinks(symbol: Symbol): SymbolLinks {
            if (symbol.flags & SymbolFlags.Transient) return <TransientSymbol>symbol;            
            if (!symbol.id) symbol.id = nextSymbolId++;
            var s = symbolLinks[symbol.id];
            if(s) { return s; } 
            else { var o = {}; symbolLinks[symbol.id] = o; return o; }
        }

        /*@ getNodeLinks :: (node: INode) => { NodeLinks<Immutable> | true } */
        function getNodeLinks(node: Node): NodeLinks {
            var node_id = node.id;
            if (!node_id) {
                node_id = nextNodeId++;
                node.id = node_id;
                /*@ local node_id_0 :: number + undefined */
                var node_id_0 = node_id;
                node_id = node_id_0;
            }
            var n = nodeLinks[node_id];
            if(n) { return n; }
            else { var o = {}; nodeLinks[<number>node_id] = o; return o; }
        }

        /*@ getSourceFile :: (node: INode + undefined) => undefined + { SourceFile<Immutable> | true } */
        function getSourceFile(node: Node): SourceFile {
            var ancestor = getAncestor(node, SyntaxKind.SourceFile);
            if (ancestor) {
                return <SourceFile> (<Node>ancestor);
            } else {
                return undefined;
            }
        }

        /*@ createType :: (flags: bitvector32) => { Type<UniqueMutable> | type_flags(flags,v) } */
        function createType(flags: TypeFlags): Type {
            /*@ result :: Type<UniqueMutable> */
            var result = newType(checker, flags); 
            result.id = typeCount++;
            return result;
        }

        /*@ createObjectType :: forall M . (kind: { bitvector32 | mask_typeflags_anonymous(v) || mask_typeflags_reference(v) }, symbol: ISymbol + undefined) => { ObjectType<M> | type_flags(kind,v) } */
        function createObjectType(kind: TypeFlags, symbol?: Symbol): ObjectType {
            var type = <ObjectType>createType(kind);
            type.symbol = symbol;
            return type;
        }

        /*@ resolveObjectTypeMembers :: (type: ObjectType<Immutable>) => { ResolvedObjectType<Immutable> | true } */
        declare function resolveObjectTypeMembers(type: ObjectType): ResolvedObjectType;

        /*@ getPropertiesOfType :: (type: IType) => { IArray<ISymbol> | true } */
        function getPropertiesOfType(type: Type): Symbol[] {
            if (type.flags & TypeFlags.ObjectType) {
                return resolveObjectTypeMembers(<ObjectType>type).properties;
            }
            return emptyArray();
        }

        /*@ getTypeListId :: (types: IArray<IType>) => { number | true } */
        declare function getTypeListId(types: Type[]);

        /*@ createTypeReference :: (target: GenericType<Immutable>, typeArguments: IArray<IType>) => { TypeReference<Immutable> | true } */
        function createTypeReference(target: GenericType, typeArguments: Type[]): TypeReference {
            var id = getTypeListId(typeArguments);
            var type = target.instantiations[id.toString()];
            if (!type) {
                assume(target.symbol);         // TODO: Remove this assumption
                /*@ type1 :: TypeReference<UniqueMutable> */
                var type1 = <TypeReference>createObjectType(TypeFlags.Reference, target.symbol);
                var tmp = target.instantiations;
                // tmp[id] = type1;            // TODO: Cannot assign a UniqueMutable variable
                type1.target = target;
                type1.typeArguments = typeArguments;
                return type1;
            }
            return type;
        }

        /*@ isTypeParameterReferenceIllegalInConstraint :: (typeReferenceNode: TypeReferenceNode<Immutable>, typeParameterSymbol: ISymbol) => { boolean | true } */
        function isTypeParameterReferenceIllegalInConstraint(typeReferenceNode: TypeReferenceNode, typeParameterSymbol: Symbol): boolean {
            var links = getNodeLinks(typeReferenceNode);
            var links_isIllegalTypeReferenceInConstraint_tmp = links.isIllegalTypeReferenceInConstraint;
            // if (links.isIllegalTypeReferenceInConstraint !== undefined) {
            if (typeof links_isIllegalTypeReferenceInConstraint_tmp !== "undefined") {
                return links_isIllegalTypeReferenceInConstraint_tmp;
            }

            // bubble up to the declaration
            /*@ currentNode :: INode */
            var currentNode: Node = typeReferenceNode;
            // forEach === exists
            /*@ _check :: (d: Declaration<Immutable>) => boolean */
            function _check(d: Declaration): boolean { return d.parent === currentNode.parent }
            var cnt = true;     // PV: adding explicit check  
            while (cnt && !forEach(typeParameterSymbol.declarations, _check)) {
                var cp = currentNode.parent;
                if (cp) { currentNode = cp; } else { cnt = false; }
            }
            // if last step was made from the type parameter this means that path has started somewhere in constraint which is illegal
            links_isIllegalTypeReferenceInConstraint_tmp = currentNode.kind === SyntaxKind.TypeParameter;
            links.isIllegalTypeReferenceInConstraint = links_isIllegalTypeReferenceInConstraint_tmp;
            return links_isIllegalTypeReferenceInConstraint_tmp;
        }

        /*@ instantiateList :: forall M T . (items: IArray<T>, mapper: TypeMapper<Immutable>, instantiator: (item: T, mapper: TypeMapper<Immutable>) => T) => MArray<T> */
        function instantiateList<T>(items: T[], mapper: TypeMapper, instantiator: (item: T, mapper: TypeMapper) => T): T[] {
            if (items && items.length) {
                var result: T[] = [];
                for (var i = 0; i < items.length; i++) {
                    result.push(instantiator(items[i], mapper));
                }
                return result;
            }
            return items;
        }

        /*@ identityMapper :: (type: IType) => IType */
        function identityMapper(type: Type): Type {
            return type;
        }

        /*@ instantiateSignature :: /\ (signature: ISignature, mapper: TypeMapper<Immutable>) => { ISignature | true } 
                                    /\ (signature: ISignature, mapper: TypeMapper<Immutable>, eraseTypeParameters: boolean) => { ISignature | true } */
        declare function instantiateSignature(signature: Signature, mapper: TypeMapper, eraseTypeParameters?: boolean): Signature;

        /*@ instantiateSymbol :: (symbol: ISymbol, mapper: TypeMapper<Immutable>) => { ISymbol | true } */
        declare function instantiateSymbol(symbol: Symbol, mapper: TypeMapper): Symbol;

        /*@ instantiateAnonymousType :: (type: ObjectType<Immutable>, mapper: TypeMapper<Immutable>) => { ObjectType<Mutable> | true } */
        declare function instantiateAnonymousType(type: ObjectType, mapper: TypeMapper): ObjectType;

        /*@ instantiateType :: (type: IType, mapper: TypeMapper<Immutable>) => { IType | true } */
        declare function instantiateType(type: Type, mapper: TypeMapper): Type;

        /*@ getAncestor :: (node: INode + undefined, kind: SyntaxKind) => undefined + INode */
        function getAncestor(node: Node, kind: SyntaxKind): Node {
            if (kind === SyntaxKind.ClassDeclaration) {
                while (typeof node !== "undefined") {
                    if (node.kind === SyntaxKind.ClassDeclaration) {
                        //return <ClassDeclaration>node;
                        return <Node>node;
                    }
                    else if (kind === SyntaxKind.EnumDeclaration      ||
                             kind === SyntaxKind.InterfaceDeclaration ||
                             kind === SyntaxKind.ModuleDeclaration    ||
                             kind === SyntaxKind.ImportDeclaration) {
                        // early exit cases - declarations cannot be nested in classes
                        return undefined;
                    }
                    else {
                        node = node.parent;
                    }
                }
            }
            else {
                while (node) {
                    if (node.kind === kind) {
                        return <Node>node;
                    }
                    else {
                        node = node.parent;
                    }
                }
            }

            return undefined;
        }
}
