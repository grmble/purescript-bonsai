"use strict";

// copy an array-ish thing (like a nodelist) to an Array
//
// the fake array needs to have a length property and
// have properties for all indices [0, length)
function copyFakeArray(fake) {
  var arr = [], i;
  for (i = 0; i < fake.length; i++) {
    arr.push(fake[i]);
  }
  return arr;
}

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

, copyFakeArray: copyFakeArray

, focusElement: function (elem) {
    elem.focus();
  }

, querySelector: function (str, elem) {
    return elem.querySelector(str);
  }

, querySelectorAll: function (str, elem) {
    return elem.querySelectorAll(str);
  }

, selectElement: function (elem) {
    elem.select();
  }

, setLocationHash: function (str, loc) {
    loc.hash = str;
  }

, requestAnimationFrame: function (fn, win) {
    if (typeof win.requestAnimationFrame == "undefined") {
      win.setTimeout(fn);
      return 0xdeadbeef;
    }
    return win.requestAnimationFrame(fn);
  }
};
