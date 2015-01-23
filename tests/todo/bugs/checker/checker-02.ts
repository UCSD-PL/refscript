/// <reference path="types.ts"/>
/// <reference path="core.ts"/>
/// <reference path="parser.ts"/>

module ts {

  /*@ alias nat = { number | v > 0 } */

  /*@ isGlobalSourceFile :: (node: INodeK) => { boolean | true } 
   */
  function isGlobalSourceFile(node: Node) {
      return node.kind === SyntaxKind.SourceFile && !isExternalModule(<SourceFile>node);
  }

}

