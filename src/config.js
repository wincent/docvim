/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import yargs from 'yargs';

export default yargs
  .version(() => require('../package').version)
  .usage('Usage: $0 [options]')
  .example('$0 -C build file.txt README.md -')
  .alias('C', 'directory')
  .default('C', '.', 'current directory')
  .describe('C', 'Change to directory before processing')
  .nargs('C', 1)
  .string('C')
  .help('h')
  .alias('h', 'help')
  .epilog('https://github.com/wincent/docvim')
  .strict()
  .argv;
