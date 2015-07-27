/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import yargs from 'yargs';

const json = require('../package');

export default yargs
  .version(json.version)
  .usage('Usage: $0 [options]')
  .example('$0 -C build file.txt README.md -')
  .alias('C', 'directory')
  .default('C', '.', 'current directory')
  .describe('C', 'Change to directory before processing')
  .nargs('C', 1)
  .string('C')
  .help('h')
  .alias('h', 'help')
  .epilog(json.homepage)
  .strict()
  .argv;
