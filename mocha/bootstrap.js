/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

require('babel/polyfill');

process.on('unhandledRejection', function(reason, promise) {
  throw reason;
});

global.expect = require('expect');
global.sinon = require('sinon');

require('babel/register')(
  JSON.parse(require('fs').readFileSync('.babelrc'))
);
