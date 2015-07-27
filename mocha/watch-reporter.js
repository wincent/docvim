/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

var Base = require('mocha').reporters.Base;

/**
 * Like the "min" reporter that comes with Mocha[0], but doesn't clear the
 * screen.
 *
 * @see https://github.com/mochajs/mocha/blob/master/lib/reporters/min.js
 * @see https://github.com/mochajs/mocha/wiki/Third-party-reporters
 */
function WatchReporter(runner) {
  Base.call(this, runner);
  runner.on('end', Base.prototype.epilogue.bind(this));
}

module.exports = WatchReporter;
