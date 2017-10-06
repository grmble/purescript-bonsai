"use strict";

const jsdom = require("jsdom");

function makeDocument(str) {
  return function () {
      const dom = new jsdom.JSDOM(str);
      return dom.window.document;
  };
};

function elementById(doc) {
  return function (id) {
    return function () {
      return doc.getElementById(id);
    }
  }
}

exports.makeDocument = makeDocument;
exports.elementById = elementById;
