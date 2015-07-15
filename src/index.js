#!/usr/bin/env node
/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

require('babel/polyfill');

import run from './run';

process.on('unhandledRejection', (reason, promise) => {
  throw reason;
});

(async function() {
  await run();
})();
