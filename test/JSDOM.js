"use strict";

const jsdom = require('jsdom');
const simulant = require('jsdom-simulant');

const jsdomOpts = {
  runScripts: 'dangerously',
  pretendToBeVisual: true,
};


exports.primitives =
  { jsdomWindow: function (str) {
      const dom = new jsdom.JSDOM(str, jsdomOpts);
      return dom.window;
    }
  , simulantFire: function (ev, elem) {
      simulant.fire(elem, ev);
    }

  };
