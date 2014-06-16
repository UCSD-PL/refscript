'use strict';

/*******************************************************************************r
/************** Extract Demo from URL ******************************************/
/*******************************************************************************/



function getDemo(name){
  var d = allDemos[name];
  var res = { "name" : d.name , "file" : name };
  return res;
}

function getDemos(ty){ 
  var a = [];
  for (var k in allDemos) { 
    if (allDemos[k].type == ty) 
      a.push(getDemo(k));
  };
  return a;
}

function getDefaultDemo(){
  var cat = allCategories[0].type;
  var ds  = getDemos(cat);
  // debugDemo = ds[1];
  return ds[1];
}

function getCategories(){
  function tx(c){return {type: c.type, name: c.name, demos: getDemos(c.type)}};
  return allCategories.map(tx);
}
  

/*******************************************************************************/
/************** Setting Up Editor **********************************************/
/*******************************************************************************/

var progEditor  = ace.edit("program");

progEditor.setTheme(editorTheme);                   // progEditor.setTheme("ace/theme/xcode");
var SrcMode     = require(editorMode).Mode;         // var SrcMode     = require("ace/mode/haskell").Mode;
progEditor.getSession().setMode(new SrcMode());
var typeTooltip = new TokenTooltip(progEditor, getAnnot);

function resizeProgEditor() {
  var w = $('#program-pane').width();
  return $('#program').width(w);
};

//listen for changes
$(window).resize(resizeProgEditor);

//set initially
resizeProgEditor();

// Resize Editor
function toggleEditorSize(x){
  var ht = 400;
  if (x.isFullScreen){
    ht = 600;
  };
  $("#program-pane").height(ht); 
  $("#program").height(ht-60);
}



/*******************************************************************************/
/** Markers For Errors *********************************************************/
/*******************************************************************************/

function errorRange(err){
  
  var row0 = err.start.line - 1;
  var col0 = err.start.column - 1;
  var row1 = err.stop.line - 1;
  var col1 = err.stop.column - 1;
 
  if (row0 == row1 && col0 == col1){
    return new Range(row0, col0, row0, col0 + 1);
  } else {
    return new Range(row0, col0, row1, col1);
  }
}

function errorMarker(editor, err){
  var r = errorRange(err);
  return editor.session.addMarker(r, "ace_step", "error");
}

function errorAceAnnot(err){
  var etext = defaultErrText;
  if (err.message) { etext = err.message; }
  var ann = { row   : err.start.line - 1
            , column: err.start.column
            , text  : etext
            , type  : "error"
            };
  return ann;
}

// Globals
var errorMarkers = [];

function setErrors(editor, errs){
  // Add Error Markers
  errorMarkers.forEach(function(m){ editor.session.removeMarker(m); });
  errorMarkers = errs.map(function(e){ return errorMarker(editor, e);});
  
  // Add Gutter Annotations
  editor.session.clearAnnotations();
  var annotations  = errs.map(errorAceAnnot);
  editor.session.setAnnotations(annotations);
}


/*******************************************************************************/
/************** URLS ***********************************************************/
/*******************************************************************************/

function isPrefix(p, q) { 
  return (p == q.slice(0, p.length)) 
}

function getQueryURL(){ 
  return 'query'; 
}

function getSrcURL(file){ 
  if (file.match("/")){
    return file;
  } else { 
    return ('demos/' + file);
  }
}



/*******************************************************************************/
/************** Queries ********************************************************/
/*******************************************************************************/

function getCheckQuery($scope){ 
  return { type    : "check",
           program : getSourceCode() 
         };
}

function getRecheckQuery($scope){
  var p = "";
  if ($scope.filePath) p = $scope.filePath;

  return { type    : "recheck", 
           program : getSourceCode(), 
           path    : p
         };
}

function getLoadQuery($scope){
  return { type    : "load", 
           path    : $scope.localFilePath
         };
}

function getSaveQuery($scope){
  return { type    : "save"
         , program : getSourceCode()
         , path    : $scope.localFilePath
         };
}

function getPermaQuery($scope){
  return { type    : "perma"
         , program : getSourceCode()
         };
}

/*******************************************************************************/
/************** Tracking Status and Source *************************************/
/*******************************************************************************/

function clearStatus($scope){
  $scope.isSafe       = false;
  $scope.isUnsafe     = false;
  $scope.isError      = false;
  $scope.isCrash      = false;
  $scope.isChecking   = false;
  $scope.isUnknown    = true ;
}

function setStatusChecking($scope){
  clearStatus($scope);
  $scope.isChecking = true;
  $scope.isUnknown  = false;
}

function setStatusResult($scope, data){
  var result          = getResult(data);
  debugResult         = result;
  clearStatus($scope);
  $scope.isChecking   = false;
  $scope.isSafe       = (result == "safe"  );
  $scope.isUnsafe     = (result == "unsafe");
  $scope.isCrash      = (result == "crash" );
  $scope.isError      = (result == "error" );
  $scope.isUnknown    = !($scope.isSafe || $scope.isError || $scope.isUnsafe || $scope.isCrash);
  $scope.filePath     = data.path;
  return result;
}

function setSourceCode($scope, srcName, srcText){
  clearStatus($scope);
  $scope.filePath       = null;             
  $scope.sourceFileName = srcName.split("/").pop(); // drop path prefix
  progEditor.getSession().setValue(srcText);  
}

function getSourceCode(){
  return progEditor.getSession().getValue();
}

/*******************************************************************************/
/************** Loading Files **************************************************/
/*******************************************************************************/
// DEAD CODE. All loading happens via server.

/*@ fileText :: (file, (string) => void) => void */
function fileText(file, k){
  var reader = new FileReader();
  reader.addEventListener("load", function(e){
    k(e.target.result);
  });
  reader.readAsText(file);
}

/*@ loadSourceFile :: (scope, file) => void */
function loadLocalFile($scope, file){
  if (window.File && window.FileList && window.FileReader && file) {
    if (file.type.match('text')) {
      fileText(file, function(srcText){ 
        setSourceCode($scope, file.name, srcText); 
      });
    } else { 
      alert("Can only load text files.");
    }
  } else {
    alert("Cannot load files: browser does not support File API");
  }
}


/*******************************************************************************/
/** Extracting JSON Results ****************************************************/
/*******************************************************************************/

function getResult(d) { 
  var res = "crash";
  if (d) {
    res = d.status; 
  }
  return res;
}

function getWarns(d){ 
  var ws = [];
  if (d && d.errors){
    var ws = d.errors.map(function(x){ 
               return x.message;
             });
  }
  return ws;
}

/*******************************************************************************/
/************** Top-Level Demo Controller **************************************/
/*******************************************************************************/

var debugQuery  = null;
var debugData   = null;
var debugResult = null;
var debugResp   = 0;
var debugFiles  = null;
var debugDemo   = null;
var debugSrcURL = null;
var debugZ      = null;

function LiquidDemoCtrl($scope, $http, $location) {

  // Start in non-fullscreen
  $scope.isFullScreen  = false; 
  $scope.embiggen      = "FullScreen";
  $scope.demoTitle     = demoTitle;
  $scope.demoSubtitle  = demoSubtitle;
  $scope.links         = allLinks;
  $scope.categories    = getCategories();
  $scope.isLocalServer = (document.location.hostname == "localhost");
  $scope.localFilePath = "";

  // For debugging
  $scope.gong =  function(s) { alert(s); };

  // Toggle fullScreen mode
  $scope.toggleFullScreen  = function(){
    $scope.isFullScreen = !$scope.isFullScreen;
    $scope.embiggen     = ($scope.embiggen == "FullScreen") ? "Shrink" : "FullScreen";
    toggleEditorSize($scope);
  };

  // LOAD a file from disk (only when isLocalServer)
  $scope.loadFromLocalPath = function(){ 
    var srcName = $scope.localFilePath;
    if (srcName){
      // alert('so you want to load' + $scope.localFilePath); 
      $http.post(getQueryURL(), getLoadQuery($scope))
           .success(function(data, status){
             debugData = data;
             if (data.program) { 
               setSourceCode($scope, srcName, data.program);
             } else if (data.error) {
               alert("Load Error " + data.error); 
             } else {
               alert("Horrors: Load Failed! " + srcName); 
             }
           })
           .error(function(data, status){
             alert("Load Error: No response for " + srcName); 
           });
    }
  };

  // SAVE a file to disk (only when isLocalServer)
  $scope.saveToLocalPath = function(){ 
    var srcName = $scope.localFilePath;
    //alert('so you want to save ' + $scope.localFilePath); 
    if (srcName) {
      $http.post(getQueryURL(), getSaveQuery($scope))
           .success(function(data, status){
             debugData = data;
             if (data.path){
               alert("Saved."); 
             } else {
               alert("Save Unsuccessful: " + data);
             }
           })
           .error(function(data, status){
             alert("Save Failed: " + data); 
           });
    }
  };

  // Clear Status when editor is changed
  progEditor.on("change", function(e){ 
    $scope.$apply(function(){
      clearStatus($scope);
    });
  });
  

  // Load a particular demo
  $scope.loadSource   = function(demo){
    debugDemo   = demo;
    var srcName = demo.file;
    var srcURL  = getSrcURL(srcName);
    debugSrcURL = srcURL;
    $http.get(srcURL)
         .success(function(srcText) { setSourceCode($scope, srcName, srcText); })
         .error(function(data, stat){ alert("Horrors: No such file! " + srcURL); })
         ;
  };

  // Initialize with Test000.hs
  debugDemo = getDefaultDemo();
  $scope.loadSource(debugDemo); //getDefaultDemo());

  // Extract demo name from URL 
  $scope.$watch('location.search()', function() {
    // debugZ  = ($location.search()).demo;
    $scope.demoName = ($location.search()).demo;
    var newDemo = {file : $scope.demoName};
    if ($scope.demoName in allDemos){
      newDemo = getDemo($scope.demoName);
    }
    $scope.loadSource(newDemo);
    }, true);

  // Update demo name in URL 
  $scope.changeTarget = function(demo) {
     $location.search('demo', demo.file);
     $scope.loadSource(demo);
  };
 
  // Change editor keybindings
  $scope.keyBindingsNone  = function (){ progEditor.setKeyboardHandler(null); };
  $scope.keyBindingsVim   = function (){ progEditor.setKeyboardHandler("ace/keyboard/vim"); };
  $scope.keyBindingsEmacs = function (){ progEditor.setKeyboardHandler("ace/keyboard/emacs"); };

  // PERMALINK
  $scope.makePermalink = function(){
    $http.post(getQueryURL(), getPermaQuery($scope))
         .success(function(data, status){
           if (data.path){
             debugData        = data;
             $scope.changeTarget({file : data.path});
           } else {
             alert("Permalink did not return link: " + data); 
           }
         })
         .error(function(data, status){
            alert("Permalink Failed: " + status); 
         });
  };


  // http://www.cleverweb.nl/javascript/a-simple-search-with-angularjs-and-php/
  function verifyQuery(query){ 
    debugQuery = query;
    setStatusChecking($scope);
    $http.post(getQueryURL(), query)
         .success(function(data, status) {
            debugResp        = debugResp + 1; 
            $scope.status    = status;
            debugData        = data;
            $scope.warns     = getWarns(data); 
            $scope.annotHtml = data.annotHtml;
            $scope.result    = setStatusResult($scope, data);
           
            // This may be "null" if liquid crashed...
            if (data) { 
              setAnnots(data.types);
              setErrors(progEditor, data.errors);
            };
            
        })
         .error(function(data, status) {
            var msg = (data || "Request failed") + status;
            alert(msg);
         });
  };
  
  $scope.verifySource   = function(){ verifyQuery(getCheckQuery($scope));   };
  $scope.reVerifySource = function(){ verifyQuery(getRecheckQuery($scope)); };
   
}

/************************************************************************/
/***** Initialize Angular ***********************************************/
/************************************************************************/

var demo = angular.module("liquidDemo", []);
demo.controller('LiquidDemoCtrl', LiquidDemoCtrl);
toggleEditorSize({isFullScreen : false });
