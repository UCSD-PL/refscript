/// <reference path="types.ts"/>
/// <reference path="core.ts"/>
/// <reference path="parser.ts"/>

module ts {

  /*@ alias nat = { number | v > 0 } */

  /*@ isGlobalSourceFile :: (node: INodeK) => { boolean | 0 < 1 } 
   */
  function isGlobalSourceFile(node: Node) {
      return node.kind === SyntaxKind.SourceFile && !isExternalModule(<SourceFile>node);
  }

}

