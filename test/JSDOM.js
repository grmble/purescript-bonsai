"use strict";

const jsdom = require('jsdom');
const simulant = require('jsdom-simulant');

const jsdomOpts = {
  runScripts: 'dangerously',
  pretendToBeVisual: true,
};

function jsdomWindow(str) {
  return function () {
      const dom = new jsdom.JSDOM(str, jsdomOpts);
      return dom.window;
  };
}

// see https://stackoverflow.com/a/27557936
function jsdomClick(doc) {
  return function (elem) {
    return function () {
      var evt = doc.createEvent('HTMLEvents');
      evt.initEvent('click', true, true);
      elem.dispatchEvent(evt);
    };
  };
}

function simulantFire(ev) {
  return function (elem) {
    return function () {
      console.log("XXXXX calling fire", elem, ev);
      simulant.fire(elem, ev);
    };
  };
}

exports.jsdomClick = jsdomClick;
exports.jsdomWindow = jsdomWindow;
exports.simulantFire = simulantFire;
