/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

require('babel/polyfill');

var fs = require('fs');
var path = require('path');

process.on('unhandledRejection', function(reason, promise) {
  throw reason;
});

global.expect = require('expect');

require('babel/register')(
  JSON.parse(
    fs.readFileSync(
      path.join(__dirname, '..', '.babelrc')
    )
  )
);
