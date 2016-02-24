// Copyright 2009 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// This benchmark is based on a JavaScript log processing module used
// by the V8 profiler to generate execution time profiles for runs of
// JavaScript applications, and it effectively measures how fast the
// JavaScript engine is at allocating nodes and reclaiming the memory
// used for old nodes. Because of the way splay trees work, the engine
// also has to deal with a lot of changes to the large tree object
// graph.


module SplayVERSION {

    declare type MSplayTree = SplayTree<Mutable>
    declare type MSplayTreeNode = SplayTreeNode<Mutable>

    interface PayloadTreeNode<M extends ReadOnly> {array: IArray<number>; string: string}
    interface PayloadTreeR<M extends ReadOnly> {left: PayloadTree<M>; right: PayloadTree<M>}
    declare type PayloadTree<M extends ReadOnly> = PayloadTreeNode<M> | PayloadTreeR<M>;

    // Configuration.
    /*@ readonly */
    let kSplayTreeSize = 8000;
    /*@ readonly */
    let kSplayTreeModifications = 80;
    /*@ readonly */
    let kSplayTreePayloadDepth = 5;

    /*@ splayTree :: MSplayTree + null */
    let splayTree:MSplayTree = null;

    function GeneratePayloadTree(depth:number, tag:string):PayloadTree<Immutable> {
        if (depth === 0) {
            return {
                array: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                string: 'String for key ' + tag + ' in leaf node'
            };
        } else {
            return {
                left: GeneratePayloadTree(depth - 1, tag),
                right: GeneratePayloadTree(depth - 1, tag)
            };
        }
    }


    function GenerateKey() {
        // The benchmark framework guarantees that Math.random is
        // deterministic; see base.js.
        return Math.random();
    }

    /*@ InsertNewNode :: () => void */
    let InsertNewNode : () => void = function() {
        let tree = splayTree;
        if (!tree) throw new Error('splayTree is null! did you forget to call SplaySetup?');
        // Insert new node with a unique key.
        let key = GenerateKey();
        while (tree.find(key) !== null) {
            key = GenerateKey();
        }
        let payload = GeneratePayloadTree(kSplayTreePayloadDepth, String(key));
        tree.insert(key, payload);
        return key;
    }



    export let SplaySetup : () => void = function() {
        splayTree = new SplayTree();
        for (let i = 0; i < kSplayTreeSize; i++) InsertNewNode();
    }


    export let SplayTearDown : () => void = function() {
        let tree = splayTree;
        if (!tree) throw new Error('splayTree is null! did you forget to call SplaySetup?');
        // Allow the garbage collector to reclaim the memory
        // used by the splay tree no matter how we exit the
        // tear down function.
        let keys = tree.exportKeys();
        splayTree = null;

        // Verify that the splay tree has the right size.
        let length = keys.length;
        if (length !== kSplayTreeSize) {
            throw new Error("Splay tree has wrong size");
        }

        // Verify that the splay tree has sorted, unique keys.
        for (let i = 0; i < length - 1; i++) {
            if (keys[i] >= keys[i + 1]) {
                throw new Error("Splay tree not sorted");
            }
        }
    }


    export let SplayRun : () => void = function() {
        let tree = splayTree;
        if (!tree) throw new Error('splayTree is null! did you forget to call SplaySetup?');
        // Replace a few nodes in the splay tree.
        for (let i = 0; i < kSplayTreeModifications; i++) {
            console.log(".");
            let key = InsertNewNode();
            let greatest = tree.findGreatestLessThan(key);
            if (!greatest) tree.remove(key);
            else tree.remove(greatest.key);
        }
    }



    /**
     * Constructs a Splay tree.  A splay tree is a self-balancing binary
     * search tree with the additional property that recently accessed
     * elements are quick to access again. It performs basic operations
     * such as insertion, look-up and removal in O(log(n)) amortized time.
     *
     * @constructor
     */
    class SplayTree<M extends ReadOnly> {
        constructor() {}

        /**
         * Pointer to the root node of the tree.
         *
         * @type {SplayTree.Node}
         * @private
         */
        /*@ root_ : MSplayTreeNode + null */
        private root_ : MSplayTreeNode = null;


        /**
         * @return {boolean} Whether the tree is empty.
         */
        public isEmpty() {
            return !this.root_;
        }


        /**
         * Inserts a node into the tree with the specified key and value if
         * the tree does not already contain a node with the specified key. If
         * the value is inserted, it becomes the root of the tree.
         *
         * @param {number} key Key to insert into the tree.
         * @param {*} value Value to insert into the tree.
         */
        /*@ @Mutable insert (key:number, value:top) : void */
        public insert(key:number, value) {
            let root = this.root_;
            if (!root) {
                this.root_ = new SplayTreeNode(key, value);
                return;
            }
            // Splay on the key to move the last node on the search path for
            // the key to the root of the tree.
            this.splay_(key);
            root = this.root_;
            if (!root) throw new Error('This check should never fail'); 
            if (root.key === key) {
                return;
            }
            let node = <MSplayTreeNode>(new SplayTreeNode(key, value));
            if (key > root.key) {
                node.left = root;
                node.right = root.right;
                (<MSplayTreeNode>root).right = null;
            } else {
                node.right = root;
                node.left = root.left;
                (<MSplayTreeNode>root).left = null;
            }
            this.root_ = node;
        }


        /**
         * Removes a node with the specified key from the tree if the tree
         * contains a node with this key. The removed node is returned. If the
         * key is not found, an exception is thrown.
         *
         * @param {number} key Key to find and remove from the tree.
         * @return {SplayTree.Node} The removed node.
         */
        /*@ @Mutable remove (key:number) : MSplayTreeNode */
        public remove(key) {
            let root = this.root_;
            if (!root) {
                throw new Error('Key not found: ' + key);
            }
            this.splay_(key);
            root = this.root_;
            if (!root) throw new Error('This check should never fail'); 
            if (root.key !== key) {
                throw new Error('Key not found: ' + key);
            }
            let removed = root;
            if (!root.left) {
                this.root_ = root.right;
            } else {
                let right = root.right;
                this.root_ = root.left;
                // Splay to make sure that the new root has an empty right child.
                this.splay_(key);
                // Insert the original right child as the right child of the new
                // root.
                root = this.root_;
                if (!root) throw new Error('This check should never fail'); 
                (<MSplayTreeNode>root).right = right;
            }
            return removed;
        }


        /**
         * Returns the node having the specified key or null if the tree doesn't contain
         * a node with the specified key.
         *
         * @param {number} key Key to find in the tree.
         * @return {SplayTree.Node} Node having the specified key.
         */
        /*@ @Mutable find (key:number) : MSplayTreeNode + null */
        public find(key) {
            let root = this.root_;
            if (!root) {
                return null;
            }
            this.splay_(key);
            root = this.root_;
            if (!root) throw new Error('This check should never fail'); 
            return root.key === key ? root : null;
        }


        /**
         * @return {SplayTree.Node} Node having the maximum key value.
         */
        /*@ findMax (opt: MSplayTreeNode + null) : MSplayTreeNode + null */
        public findMax(opt_startNode) {
            let root = this.root_;
            if (!root) {
                return null;
            }
            let current = opt_startNode || root;
            let right = current.right;
            while (right) {
                current = right;
                right = current.right;
            }
            return current;
        }


        /**
         * @return {SplayTree.Node} Node having the maximum key value that
         *     is less than the specified key value.
         */
        /*@ @Mutable findGreatestLessThan (key: number) : MSplayTreeNode + null */
        public findGreatestLessThan(key) {
            let root = this.root_;
            if (!root) {
                return null;
            }
            // Splay on the key to move the node with the given key or the last
            // node on the search path to the top of the tree.
            this.splay_(key);
            root = this.root_;
            if (!root) throw new Error('This check should never fail'); 
            // Now the result is either the root node or the greatest node in
            // the left subtree.
            if (root.key < key) {
                return root;
            } else if (root.left) {
                return this.findMax(root.left);
            } else {
                return null;
            }
        }


        /**
         * @return {Array<*>} An array containing all the keys of tree's nodes.
         */
        public exportKeys() {
            /*@ result :: MArray<number> */
            let result:number[] = [];
            let root = this.root_;
            /*@ f :: (MSplayTreeNode) => void */ 
            let f = function (node) { result.push(node.key); }
            if (root) {
                root.traverse_(f);
            }
            return <MArray<number>>result;
        }


        /**
         * Perform the splay operation for the given key. Moves the node with
         * the given key to the top of the tree.  If no node has the given
         * key, the last node on the search path is moved to the top of the
         * tree. This is the simplified top-down splaying algorithm from:
         * "Self-adjusting Binary Search Trees" by Sleator and Tarjan
         *
         * @param {number} key Key to splay the tree on.
         * @private
         */
        /*@ @Mutable splay_ (key:number) : void */
        public splay_(key) {
            let root = this.root_;
            if (!root) {
                return;
            }
            // Create a dummy node.  The use of the dummy node is a bit
            // counter-intuitive: The right child of the dummy node will hold
            // the L tree of the algorithm.  The left child of the dummy node
            // will hold the R tree of the algorithm.  Using a dummy node, left
            // and right will always be nodes and we avoid special cases.
            /*@ dummy :: MSplayTreeNode */
            let dummy:MSplayTreeNode=new SplayTreeNode(-1, null);
            let left:MSplayTreeNode=dummy;
            let right:MSplayTreeNode=dummy;
            let current = <MSplayTreeNode>root;
            let shouldBreak = false;
            while (!shouldBreak) {
                if (key < current.key) {
                    let currleft = current.left;
                    if (!currleft) {
                        shouldBreak = true;
                    } else {
                        if (key < currleft.key) {
                            // Rotate right.
                            let tmp = <MSplayTreeNode>currleft;
                            current.left = tmp.right;
                            tmp.right = current;
                            current = tmp;
                            currleft = current.left;
                        }
                        if (!currleft) {
                            shouldBreak = true;
                        } else {
                            // Link right.
                            right.left = current;
                            right = current;
                            current = <MSplayTreeNode>currleft;
                        }
                    }
                } else if (key > current.key) {
                    let currright = current.right;
                    if (!currright) {
                        shouldBreak = true;
                    } else {
                        if (key > currright.key) {
                            // Rotate left.
                            let tmp = <MSplayTreeNode>currright;
                            current.right = tmp.left;
                            tmp.left = current;
                            current = tmp;
                            currright = current.right;
                        }
                        if (!currright) {
                            shouldBreak = true;
                        } else {
                            // Link left.
                            left.right = current;
                            left = current;
                            current = <MSplayTreeNode>currright;
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

    /**
     * Constructs a Splay tree node.
     *
     * @param {number} key Key.
     * @param {*} value Value.
     */
    class SplayTreeNode<M extends ReadOnly> {
        public key:number;
        public value;
        constructor(key:number, value) { this.key = key; this.value = value; }

        /**
         * @type {SplayTree.Node}
         */
        /*@ left : MSplayTreeNode + null */
        public left : MSplayTreeNode = null;


        /**
         * @type {SplayTree.Node}
         */
        /*@ right : MSplayTreeNode + null */
        public right : MSplayTreeNode = null;


        /**
         * Performs an ordered traversal of the subtree starting at
         * this SplayTree.Node.
         *
         * @param {function(SplayTree.Node)} f Visitor function.
         * @private
         */
        /*@ traverse_ (f: (arg:MSplayTreeNode) => top) : void */
        public traverse_(f : (arg:MSplayTreeNode) => any) {
            /*@ local current :: MSplayTreeNode + null */
            let current = this;
            while (current) {
                let left = current.left;
                if (left) left.traverse_(f);
                f(current);
                current = current.right;
            }
        }
    }
}
