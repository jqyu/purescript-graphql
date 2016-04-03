"use strict";

// module Rad.Typeable

exports.mkTypeableExists = function(dict) {
  return function(fa) {
    return { rep: fa, dict: dict }
  }
};

exports.runTypeableExists = function(f) {
  return function(fa) {
    return f(fa.dict)(fa.rep);
  }
};
