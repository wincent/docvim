/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import path from 'path';
import yargs from 'yargs';

const json = require('../package');

const options = yargs
  .usage('Usage: $0 [option...] [outfile...]')
  .wrap(Math.min(yargs.terminalWidth(), 90))
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
  .strict();

const config = options.argv;

if (Array.isArray(config.directory)) {
  config.directory = config.directory[config.directory.length - 1];
}

config.output = config._;
if (!config.output.length) {
  // When no explicit outputs, we output in Vim help format to standard out.
  config.output.push({format: 'help', destination: null});
} else {
  const errors = [];
  config.output.forEach(output => {
    switch (path.extname(output).toLowerCase()) {
      case '.txt':
        config.output.push({format: 'help', destination: output});
        break;
      case '.md':
        config.output.push({format: 'markdown', destination: output});
        break;
      default:
        errors.push(
          'Bad filename `' + output + '`: a ".txt" or ".md" extension required'
        );
    }
  });
  if (errors.length) {
    options.showHelp();
    errors.forEach(error => console.error(error));
    process.exit(1);
  }
}

export default config;
