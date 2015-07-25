/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

const MarkdownVisitor = require('../MarkdownVisitor');
const dedent = require('../dedent');
const lex = require('../lex');
const parse = require('../parse');

describe('MarkdownVisitor', () => {
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
    const visitor = new MarkdownVisitor(ast);
    const result = visitor.visit();
    // BUG: we're swallowing necessary whitespace before "We should" below
    const output = dedent(`
      > Here is a block-quote. You should read it. # This is not considered a heading. And ${'```'} <-- that isn't a "pre fence"

      # Code samples

      ${'```'}
      let g:example=1
      let g:other=2
      ${'```'}

      ## More stuff

      <em><a name="arbitrary-link-target" />arbitrary-link-target</em>We should probably <strong>[g:LinkToOtherStuff](#g:LinkToOtherStuff)</strong>.
    `).trim();
    expect(result).toEqual(output);
  });
});
