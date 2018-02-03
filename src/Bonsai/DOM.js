"use strict";


exports.primitives =
{ "window": (typeof window === "undefined") ? undefined : window

, elementById: function (id, doc) {
    return doc.getElementById(id);
  }

, appendChild: function (child, parent) {
    return parent.appendChild(child);
  }

, clearElement: function (elem) {
    elem.innerHTML = "";
  }

, focusElement: function (elem) {
    elem.focus();
  }

, querySelector: function (str, elem) {
    return elem.querySelector(str);
  }

, querySelectorAll: function (str, elem) {
    const nl =  elem.querySelectorAll(str);
    var arr = [], i;
    for (i = 0; i < nl.length; i++) {
      arr.push(nl[i]);
    }
    return arr;
  }

, selectElement: function (elem) {
    elem.select();
  }

, requestAnimationFrame: function (fn, win) {
    if (typeof win.requestAnimationFrame == "undefined") {
      win.setTimeout(fn);
      return 0xdeadbeef;
    }
    return win.requestAnimationFrame(fn);
  }
};
