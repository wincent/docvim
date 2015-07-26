/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Promise from 'bluebird';
import fs from 'fs';
import path from 'path';

const readdir = Promise.promisify(fs.readdir);
const stat = Promise.promisify(fs.stat);

const BLACKLIST = new Set(['node_modules']);

export default async function getFiles(directory: string): Array<string> {
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
