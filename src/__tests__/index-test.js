/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

const Promise = require('bluebird');
const fs = require('fs');
const getFiles = require('../getFiles');
const lex = require('../lex');
const parse = require('../parse');

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
