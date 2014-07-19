Benchmarks TODO
===============

For each directory in:

+  dummy
+  ace
+  d3
+  underscore

get the files to check with 

  tsc --noImplicitAny

Using CATS
------------

**Install**

Follow these (directions)[https://github.com/jbaron/cats]
      
    git clone https://github.com/jbaron/cats.git
    cd cats
    npm install nodewebkit
    
Add the following to package.json

    ,
    "scripts": {
      "start": "nodewebkit"
    }

**Run**

    cd /path/to/cats
    npm start


**Open Project**

    /path/to/Refscript/benchmarks/dummy

**Edit**

    tinker around with it to add bugs and see them get caught.


