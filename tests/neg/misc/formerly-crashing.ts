// This benchmark is based on a JavaScript log processing module used
// by the V8 profiler to generate execution time profiles for runs of
// JavaScript applications, and it effectively measures how fast the
// JavaScript engine is at allocating nodes and reclaiming the memory
// used for old nodes. Because of the way splay trees work, the engine
// also has to deal with a lot of changes to the large tree object
// graph.

module SplayVERSION {
    /**
     * A splay tree is a self-balancing binary
     * search tree with the additional property that recently accessed
     * elements are quick to access again. 
     */
    class SplayTree {
        /*@ new() => void */
        constructor() {}

        // Pointer to the root node of the tree.
        /*@ root_ : [#Mutable] SplayTreeNode<Mutable> + null */
        private root_ : SplayTreeNode = null;

        /**
         * Perform the splay operation for the given key. Moves the node with
         * the given key to the top of the tree.  If no node has the given
         * key, the last node on the search path is moved to the top of the
         * tree. This is the simplified top-down splaying algorithm from:
         * "Self-adjusting Binary Search Trees" by Sleator and Tarjan
         *
         * @param {number} key Key to splay the tree on.
         */
        /*@ splay_ : (number): { void | true } */
        public splay_(key) {
            var root = this.root_;
            if (!root) { return; }

            // Create a dummy node.  The use of the dummy node is a bit
            // counter-intuitive: The right child of the dummy node will hold
            // the L tree of the algorithm.  The left child of the dummy node
            // will hold the R tree of the algorithm.  Using a dummy node, left
            // and right will always be nodes and we avoid special cases.
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
                } else if (key > current.key) {
                    var currright = current.right;
                    if (!currright) {
                        shouldBreak = true;
                    } else {
                        if (key > currright.key) {
                            // Rotate left.
                            /*@ tmp :: SplayTreeNode<Mutable> */
                            var tmp = currright;
                            current.right = tmp.left;
                            tmp.left = current;
                            current = tmp;
                            if (!currright) {
                                shouldBreak = true;
                            }
                        }
                        if (!shouldBreak) {
                            /*@ foo :: SplayTreeNode<Mutable> */
                            var foo = currright;
                            // Link left.
                            left.right = current;
                            left = current;
                            current = foo;
                        }
                    }
                } else {
                    shouldBreak = true;
                }
            }
            // Assemble.
            left.right = current.left;
            right.left = current.right;
            current.left = dummy.right;
            current.right = dummy.left;
            this.root_ = current;
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
