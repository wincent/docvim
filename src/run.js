/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Promise from 'bluebird';
import config from './config';
import fs from 'fs';
import getFiles from './getFiles';
import lex from './lex';
import parse from './parse';

const readFile = Promise.promisify(fs.readFile);

export default async function run(): void {
  const directory = Array.isArray(config.directory) ?
    config.directory[config.directory.length - 1] :
    config.directory;

  const files = await getFiles(directory);
  const asts = await* files.map(async filename => {
    const contents = await readFile(filename);
    return parse(lex(contents.toString()));
  });
}
