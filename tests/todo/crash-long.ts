
module SplayVERSION {
    class SplayTree {
        /*@ new() => void */
        constructor() {}

        // Pointer to the root node of the tree.
        /*@ root_ : [#Mutable] SplayTreeNode<Mutable> + null */
        private root_ : SplayTreeNode = null;

        public splay_(key) {
            var root = this.root_;
            var dummy:SplayTreeNode=new SplayTreeNode(-1);
            var left:SplayTreeNode=dummy;
            var right:SplayTreeNode=dummy;
            /*@ current :: SplayTreeNode<Mutable> */
            var current = root;
            var shouldBreak = false;
            while (!shouldBreak) {
                if (key < current.key) {
                    var currleft = current.left;
                    if (!currleft) {
                        shouldBreak = true;
                    } else {
                        if (key < currleft.key) {
                            // Rotate right.
                            /*@ tmp :: SplayTreeNode<Mutable> */
                            var tmp = currleft;
                            currleft = tmp.right;
                            tmp.right = current;
                            current = tmp;
                            if (!currleft) {
                                shouldBreak = true;
                            }
                        }
                        if (!shouldBreak) {
                            /*@ foo :: SplayTreeNode<Mutable> */
                            var foo = currleft;
                            // Link right.
                            right.left = current;
                            right = current;
                            current = foo;
                        }
                    }
                } 
            }
        }
    }

    class SplayTreeNode {
        public key:number;

        /*@ new (key:number) => void */
        constructor(key:number) {
            this.key = key;
        }

        /*@ left : [#Mutable] SplayTreeNode<Mutable> + null */
        public left : SplayTreeNode = null;

        /*@ right : [#Mutable] SplayTreeNode<Mutable> + null */
        public right : SplayTreeNode = null;

    }
}
