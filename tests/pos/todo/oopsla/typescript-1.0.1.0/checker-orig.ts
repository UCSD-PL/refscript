/// <reference path="types.ts"/>
/// <reference path="core.ts"/>
/// <reference path="scanner.ts"/>
/// <reference path="parser.ts"/>
/// <reference path="binder.ts"/>
/// <reference path="emitter.ts"/>

module ts {

    var nextSymbolId = 1;
    var nextNodeId = 1;
    var nextMergeId = 1;

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

        var typeCount = 0;

        var emptyArray: any[] = [];

        var emptySymbols: SymbolTable = {};

        var compilerOptions = program.getCompilerOptions();

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

        var mergedSymbols: Symbol[] = [];
        var symbolLinks: SymbolLinks[] = [];
        var nodeLinks: NodeLinks[] = [];

        declare function createSymbol(flags: SymbolFlags, name: string): Symbol;

        function getExcludedSymbolFlags(flags: SymbolFlags): SymbolFlags {
            var result: SymbolFlags = 0;
            if (flags & SymbolFlags.Variable) result |= SymbolFlags.VariableExcludes;
            if (flags & SymbolFlags.Property) result |= SymbolFlags.PropertyExcludes;
            if (flags & SymbolFlags.EnumMember) result |= SymbolFlags.EnumMemberExcludes;
            if (flags & SymbolFlags.Function) result |= SymbolFlags.FunctionExcludes;
            if (flags & SymbolFlags.Class) result |= SymbolFlags.ClassExcludes;
            if (flags & SymbolFlags.Interface) result |= SymbolFlags.InterfaceExcludes;
            if (flags & SymbolFlags.Enum) result |= SymbolFlags.EnumExcludes;
            if (flags & SymbolFlags.ValueModule) result |= SymbolFlags.ValueModuleExcludes;
            if (flags & SymbolFlags.Method) result |= SymbolFlags.MethodExcludes;
            if (flags & SymbolFlags.GetAccessor) result |= SymbolFlags.GetAccessorExcludes;
            if (flags & SymbolFlags.SetAccessor) result |= SymbolFlags.SetAccessorExcludes;
            if (flags & SymbolFlags.TypeParameter) result |= SymbolFlags.TypeParameterExcludes;
            if (flags & SymbolFlags.Import) result |= SymbolFlags.ImportExcludes;
            return result;
        }

        function recordMergedSymbol(target: Symbol, source: Symbol) {
            if (!source.mergeId) source.mergeId = nextMergeId++;
            mergedSymbols[source.mergeId] = target;
        }

        function getSymbolLinks(symbol: Symbol): SymbolLinks {
            if (symbol.flags & SymbolFlags.Transient) return <TransientSymbol>symbol;
            if (!symbol.id) symbol.id = nextSymbolId++;
            return symbolLinks[symbol.id] || (symbolLinks[symbol.id] = {});
        }

        function getNodeLinks(node: Node): NodeLinks {
            if (!node.id) node.id = nextNodeId++;
            return nodeLinks[node.id] || (nodeLinks[node.id] = {});
        }

        function getSourceFile(node: Node): SourceFile {
            return <SourceFile>getAncestor(node, SyntaxKind.SourceFile);
        }

        function createType(flags: TypeFlags): Type {
            var result = new Type(checker, flags);
            result.id = typeCount++;
            return result;
        }

        function createObjectType(kind: TypeFlags, symbol?: Symbol): ObjectType {
            var type = <ObjectType>createType(kind);
            type.symbol = symbol;
            return type;
        }

        declare function resolveObjectTypeMembers(type: ObjectType): ResolvedObjectType;

        function getPropertiesOfType(type: Type): Symbol[] {
            if (type.flags & TypeFlags.ObjectType) {
                return resolveObjectTypeMembers(<ObjectType>type).properties;
            }
            return emptyArray;
        }

        declare function getTypeListId(types: Type[]);

        function createTypeReference(target: GenericType, typeArguments: Type[]): TypeReference {
            var id = getTypeListId(typeArguments);
            var type = target.instantiations[id];
            if (!type) {
                type = target.instantiations[id] = <TypeReference>createObjectType(TypeFlags.Reference, target.symbol);
                type.target = target;
                type.typeArguments = typeArguments;
            }
            return type;
        }

        function isTypeParameterReferenceIllegalInConstraint(typeReferenceNode: TypeReferenceNode, typeParameterSymbol: Symbol): boolean {
            var links = getNodeLinks(typeReferenceNode);
            if (links.isIllegalTypeReferenceInConstraint !== undefined) {
                return links.isIllegalTypeReferenceInConstraint;
            }

            // bubble up to the declaration
            var currentNode: Node = typeReferenceNode;
            // forEach === exists
            while (!forEach(typeParameterSymbol.declarations, d => d.parent === currentNode.parent)) {
                currentNode = currentNode.parent;
            }
            // if last step was made from the type parameter this means that path has started somewhere in constraint which is illegal
            links.isIllegalTypeReferenceInConstraint = currentNode.kind === SyntaxKind.TypeParameter;
            return links.isIllegalTypeReferenceInConstraint;
        }


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

        function identityMapper(type: Type): Type {
            return type;
        }

        declare function instantiateSignature(signature: Signature, mapper: TypeMapper, eraseTypeParameters?: boolean): Signature;

        declare function instantiateSymbol(symbol: Symbol, mapper: TypeMapper): Symbol;

        declare function instantiateAnonymousType(type: ObjectType, mapper: TypeMapper): ObjectType;

        declare function instantiateType(type: Type, mapper: TypeMapper): Type;

        function getAncestor(node: Node, kind: SyntaxKind): Node {
            switch (kind) {
                // special-cases that can be come first
                case SyntaxKind.ClassDeclaration:
                    while (node) {
                        switch (node.kind) {
                            case SyntaxKind.ClassDeclaration:
                                return <ClassDeclaration>node;
                            case SyntaxKind.EnumDeclaration:
                            case SyntaxKind.InterfaceDeclaration:
                            case SyntaxKind.ModuleDeclaration:
                            case SyntaxKind.ImportDeclaration:
                                // early exit cases - declarations cannot be nested in classes
                                return undefined;
                            default:
                                node = node.parent;
                                continue;
                        }
                    }
                    break;
                default:
                    while (node) {
                        if (node.kind === kind) {
                            return node;
                        }
                        else {
                            node = node.parent;
                        }
                    }
                    break;
            }

            return undefined;
        }
}
