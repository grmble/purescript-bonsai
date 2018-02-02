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

function simulantFire(ev) {
  return function (elem) {
    return function () {
      simulant.fire(elem, ev);
    };
  };
}

exports.jsdomWindow = jsdomWindow;
exports.simulantFire = simulantFire;
