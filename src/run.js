/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Promise from 'bluebird';
import fs from 'fs';
import lex from './lex';
import parse from './parse';
import path from 'path';

const readFile = Promise.promisify(fs.readFile);
const readdir = Promise.promisify(fs.readdir);
const stat = Promise.promisify(fs.stat);

const BLACKLIST = new Set(['node_modules']);

// TODO: instead of getting all the filenames, then processing all the files,
// set it up as a pipeline, getting filename, processing it, then next etc
async function getFiles(directory: string): Array<string> {
  const filenames = await readdir(directory)
    .filter(filename => {
      return (
        !filename.startsWith('.') &&
        !BLACKLIST.has(filename)
      );
    })
    .map(filename => path.join(directory, filename));

  const result = [];
  await* filenames.map(async filename => {
    let info;
    try {
      info = await stat(filename);
    } catch(error) {
      return;
    }
    if (info.isDirectory()) {
      const files = await getFiles(filename);
      result.push(...files);
    } else if (path.extname(filename) === '.vim') {
      result.push(filename);
    }
  });
  return result;
}

function process(input: string): Object {
  const tokens = lex(input);
  return parse(tokens);
}

export default async function run(): void {
  const files = await getFiles('.');
  await* files.map(async filename => {
    const contents = await readFile(filename);
    process(contents.toString());
  });
}
