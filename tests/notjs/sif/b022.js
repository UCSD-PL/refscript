var hostname = 'http://www.example.com';
var formSubmitURL = 'http://www.example.com';  // not http://www.example.com/foo/auth.cgi
var httprealm = null;
var username = UNKNOWN_STRING;
var password;
 
try {
    // Get Login Manager 
    var myLoginManager = Components.classes["@mozilla.org/login-manager;1"].
        getService(Components.interfaces.nsILoginManager);
       
    // Find users for the given parameters
    var logins = myLoginManager.findLogins({}, hostname, formSubmitURL, httprealm);
       
    // Find user from returned array of nsILoginInfo objects
    for (var i = 0; i < logins.length; i++) {
        if (username == logins[i].username) {
            password = logins[i].password;
            break;
        }
    }
}
 
catch(ex) {
    // This will only happen if there is no nsILoginManager component class
}

password;