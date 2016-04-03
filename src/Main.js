"use strict";

// module Main

exports.timerStart = function(x) {
  return function() {
    console.time(x)
  }
}

exports.timerEnd = function(x) {
  return function() {
    console.timeEnd(x)
  }
}
