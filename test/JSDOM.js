"use strict";

const jsdom = require("jsdom");

function jsdomWindow(str) {
  return function () {
      const dom = new jsdom.JSDOM(str);
      return dom.window;
  };
};

exports.jsdomWindow = jsdomWindow;
