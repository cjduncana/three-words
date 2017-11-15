'use strict';

const Promise = require('bluebird');

const Elm = require('./elm.js');

const test = Elm.Test.worker();

exports.getThreeWords = function(key, position) {
  return new Promise((resolve) => {
    test.ports.inbound.send({ key, position });
    test.ports.outbound.subscribe(resolve);
  });
};

exports.getPosition = function(key, threeWords) {
  return new Promise((resolve) => {
    test.ports.inbound.send({ key, threeWords });
    test.ports.outbound.subscribe(resolve);
  });
};
