/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import yargs from 'yargs';

const json = require('../package');

const config = yargs
  .usage('Usage: $0 [option...] [outfile...]')
  .example('$0 -C build file.txt README.md')
  .alias('C', 'directory')
  .default('C', '.', 'current directory')
  .describe('C', 'Change to directory before processing')
  .nargs('C', 1)
  .string('C')
  .alias('d', 'debug')
  .boolean('d')
  .describe('d', 'Print debug information to standard error')
  .help('h')
  .alias('h', 'help')
  .version(json.version)
  .epilog(json.homepage)
  .strict()
  .argv;

if (Array.isArray(config.directory)) {
  config.directory = config.directory[config.directory.length - 1];
}

export default config;
