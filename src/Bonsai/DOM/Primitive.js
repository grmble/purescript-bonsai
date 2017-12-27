"use strict";

exports.window = function () {
  return window;
};

exports.document = function (win) {
  return function () {
    return win.document;
  };
};

exports.primElementById = function (id) {
  return function (doc) {
    return function () {
      return doc.getElementById(id);
    };
  };
};

exports.appendChild = function (child) {
  return function (parent) {
    return function () {
      parent.append(child);
    };
  };
};

exports.clearElement = function (elem) {
  return function () {
    elem.innerHTML = '';
  };
};

exports.focusElement = function (elem) {
  return function () {
    elem.focus();
  };
};

exports.requestAnimationFrame = function (eff) {
  return function (win) {
    return function () {
      if (typeof win.requestAnimationFrame === "undefined") {
        console.log("NO REQUEST ANIMATION FRAME");
        win.setTimeout(eff);
        return 0xdeadbeef;
      }
      return win.requestAnimationFrame(eff);
    };
  };
};

exports.selectInputElementText = function (elem) {
  return function () {
    elem.select();
  };
};

exports.textContent = function (elem) {
  return function () {
    return elem.textContent;
  };
};
