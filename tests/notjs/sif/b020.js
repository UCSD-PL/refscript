var passwordManager = Components.classes["@mozilla.org/login-manager;1"].getService(
    Components.interfaces.nsILoginManager
);

passwordManager.findLogins();