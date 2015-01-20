/// <reference path="types.ts"/>
/// <reference path="core.ts"/>

module ts {

  /*@ alias nat = { number | v > 0 } */

  /*@ nextSymbolId :: nat */
  var nextSymbolId = 1;

  /*@ nextNodeId :: nat */
  var nextNodeId = 1;

  /*@ nextMergeId :: nat */
  var nextMergeId = 1;


  /*@ getAncestor :: (node: INodeK, kind: SyntaxKind) 
                  => undefined + INode 
   */
  function getAncestor(node: Node, kind: SyntaxKind): Node {

    if (kind === SyntaxKind.ClassDeclaration) {

      while (typeof node !== "undefined") {

        // CAST :: forall T . (c: T, x: T) => T
        // Node/p + undefined <: Node/K 

        var node1 = <Node> node;

        if (node1.kind === SyntaxKind.ClassDeclaration) {

          return <ClassDeclaration>node1;

        }
        else {
          node = node.parent;
        }
        
      }

    }

    return undefined;
  }

}

