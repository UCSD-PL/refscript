var nsLoginInfo = new Components.Constructor(
    "@mozilla.org/login-manager/loginInfo;1",
    Components.interfaces.nsILoginInfo,
    "init"
);
       
var loginInfo = new nsLoginInfo(
    hostname, formSubmitURL, httprealm, username, password, usernameField, passwordField
);

loginInfo;