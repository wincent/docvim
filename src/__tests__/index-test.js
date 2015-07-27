/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

import Promise from 'bluebird';
import fs from 'fs';
import getFiles from '../getFiles';
import lex from '../lex';
import parse from '../parse';

const readFile = Promise.promisify(fs.readFile);

describe('Integration tests', () => {
  it('produces output', async function(done) {
    const files = await getFiles('fixtures');
    const asts = await* files.map(async filename => {
      const contents = await readFile(filename);
      return parse(lex(contents.toString()));
    });

    // TODO: actual assertions here
    done();
  });
});
