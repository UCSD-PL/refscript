function myObserver()
{
  this.register();
}
myObserver.prototype = {
  observe: function(subject, topic, data) {
     // Do your stuff here.
     PROBE(1);
  },
  register: function() {
    var observerService = Components.classes["@mozilla.org/observer-service;1"]
                          .getService(Components.interfaces.nsIObserverService);
    observerService.addObserver(this, "myTopicID", false);
  },
  unregister: function() {
    var observerService = Components.classes["@mozilla.org/observer-service;1"]
                            .getService(Components.interfaces.nsIObserverService);
    observerService.removeObserver(this, "myTopicID");
  }
}

var observer = new myObserver();
