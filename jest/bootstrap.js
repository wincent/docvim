/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

require.requireActual('babel/polyfill');

process.on('unhandledRejection', (reason, promise) => {
  throw reason;
});
