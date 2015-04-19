
if (!Function.prototype.bind) {
  Function.prototype.bind = function (oThis) {
    if (typeof this !== 'function') {
      throw new Error('Function.prototype.bind - what is trying to be bound is not callable');
    }
    var aArgs = Array.prototype.slice.call(arguments, 1);
    var fToBind = this;
    var fNoop = function () {};
    var fBound = function () {
      return fToBind.apply(
        this instanceof fNoop && oThis ? this : oThis, 
        aArgs.concat(Array.prototype.slice.call(arguments)));
    };

    fNoop.prototype = this.prototype;
    fToBind.prototype = new fNoop();
    return fBound;
  };
}

//
// Trigger Event Implementation
//
function triggerEvent (node, eventName) {
  var doc;
  if (node.ownerDocument)
    doc = node.ownerDocument;
  else if (node.nodeType == 9)
    doc = node;
  else
    throw new Error("Invalid node passed to fireEvent: " + node.id);

  if (node.dispatchEvent) {
    var eventClass = "";
    switch (eventName) {
      case "click":
      case "mousedown":
      case "mouseup":
        eventClass = "MouseEvents";
        break;

      case "focus":
      case "change":
      case "blur":
      case "select":
        eventClass = "HTMLEvents";
        break;

      default:
        throw "fireEvent: Couldn't find an event class for event '" + eventName + "'.";
        break;
    }

    var event = doc.createEvent(eventClass);
    var bubbles = eventName == "change" ? false : true;
    event.initEvent(eventName, bubbles, true);
    event.synthetic = true;
    node.dispatchEvent(event, true);
  } else if (node.fireEvent) {
    var event = doc.createEventObject();
    event.synthetic = true;
    node.fireEvent("on" + eventName, event);
  }
};

//
// set global variables
//
window.isHeadless = !!window.mochaPhantomJS;
window.expect = chai.expect;
window.assert = chai.assert;
mocha.setup('bdd');
