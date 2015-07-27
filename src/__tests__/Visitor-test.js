/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

import Visitor from '../Visitor';
import dedent from '../dedent';
import lex from '../lex';
import parse from '../parse';

describe('Visitor', () => {
  it('visits', () => {
    const input = dedent(`
      let g:random=1
      ""
      " @plugin Plugin Description of the plug-in
      "
      " > Here is a block-quote.
      " > You should read it.
      " > # This is not considered a heading.
      " > And ${'```'} <-- that isn't a "pre fence"
      "
      " # Code samples
      "
      " ${'```'}
      " let g:example=1
      " let g:other=2
      " ${'```'}
      "
      " ## More stuff
      "
      " *arbitrary-link-target*
      " We should probably |g:LinkToOtherStuff|.
      function! s:NoLongerInTheDocBlock()
    `);
    const ast = parse(lex(input));
    const visitor = new Visitor(ast);
    const result = visitor.visit();
    expect(result).toEqual({});
  });
});
