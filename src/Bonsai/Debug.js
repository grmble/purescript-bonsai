"use strict";
// Heaviliy inspirec by purescript-debug
// Alias require to prevent webpack or browserify from actually requiring.
var req = typeof module === "undefined" ? undefined : module.require;
var util = req === undefined ? undefined : req("util");


exports.logObj = function (msg) {
  return function (obj) {
    return function () {
      // node only recurses two levels into an object before printing
      // "[object]" for further objects when using console.log()
      if (util !== undefined) {
        console.log(msg, util.inspect(obj, { depth: null, colors: true }));
      } else {
        console.log(msg, obj);
      }
      return obj;
    };
  };
};



exports.logJson = function (msg) {
  return function (obj) {
    return function () {
      console.log(msg, JSON.stringify(obj));
      return obj;
    };
  };
};

exports.logJsonObj = function (msg) {
  return function (obj) {
    return function () {
      // node only recurses two levels into an object before printing
      // "[object]" for further objects when using console.log()
      if (util !== undefined) {
        console.log(msg, JSON.stringify(obj), util.inspect(obj, { depth: null, colors: true }));
      } else {
        console.log(msg, JSON.stringify(obj), obj);
      }
      return obj;
    };
  };
};

exports.startTiming = function () {
  return new Date();
};

exports.logTiming = function (msg) {
  return function (start) {
    return function () {
      var end = new Date();
      console.log(msg + " " + (end - start) + "ms");
    };
  };
};
