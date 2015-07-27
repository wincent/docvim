/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

const SymbolVisitor = require('../SymbolVisitor');
const farmhash = require('farmhash');

describe('SymbolVisitor', () => {
  beforeEach(() => {
    sinon.stub(farmhash, 'fingerprint32', (input) => `[hash:${input}]`);
  });

  afterEach(() => {
    farmhash.fingerprint32.restore();
  });

  let ast = {
    children: [
      {
        content: 'g:FritzNob',
        name: 'LINK_TARGET',
      },
      {
        content: 'meta-stuff',
        name: 'LINK_TARGET',
      },
    ],
    name: 'TRANSLATION_UNIT',
  };

  it('returns a table of found symbols', () => {
    expect(new SymbolVisitor(ast).visit().table).toEqual({
      'g:FritzNob': '[hash:g:FritzNob]-g-fritznob',
      'meta-stuff': '[hash:meta-stuff]-meta-stuff',
    });
  });

  it('accumulates the symbols found in multiple ASTs', () => {
    const visitor = new SymbolVisitor(ast);
    let state = visitor.visit();
    const other = {
      children: [
        {
          content: ':Command',
          name: 'LINK_TARGET',
        },
        {
          content: '<Plug>(Thingy)',
          name: 'LINK_TARGET',
        },
      ],
      name: 'TRANSLATION_UNIT',
    };
    state = new SymbolVisitor(other).visit(state);
    expect(state.table).toEqual({
      'g:FritzNob': '[hash:g:FritzNob]-g-fritznob',
      'meta-stuff': '[hash:meta-stuff]-meta-stuff',
      ':Command': '[hash::Command]-command',
      '<Plug>(Thingy)': '[hash:<Plug>(Thingy)]-plug-thingy',
    });
  });

  it('throws on encountering a duplicate symbol', () => {
    const bad = {
      children: [
        {
          content: ':DuplicateCommand',
          name: 'LINK_TARGET',
        },
        {
          content: ':DuplicateCommand',
          name: 'LINK_TARGET',
        },
      ],
      name: 'TRANSLATION_UNIT',
    };
    expect(() => new SymbolVisitor(bad).visit())
      .toThrow('Duplicate link target `:DuplicateCommand`');
  });
});
