/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Promise from 'bluebird';
import fs from 'fs';
import getFiles from './getFiles';
import lex from './lex';
import parse from './parse';
import yargs from 'yargs';

const readFile = Promise.promisify(fs.readFile);

export default async function run(): void {
  const argv = yargs
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

  const directory = Array.isArray(argv.directory) ?
    argv.directory[argv.directory.length - 1] :
    argv.directory;

  const files = await getFiles(directory);
  const asts = await* files.map(async filename => {
    const contents = await readFile(filename);
    return parse(lex(contents.toString()));
  });
}
