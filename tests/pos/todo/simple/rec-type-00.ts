

module A {

  export interface TextRange {
    pos: number;
    end: number;
  }

  export interface Node extends TextRange {
    id?: number;                  // Unique id (used to look up NodeLinks)
    parent?: Node;                // Parent node (initialized by binding)
    textContainer?: Node;         // Next container in declaration order (initialized by binding)
  }
}
