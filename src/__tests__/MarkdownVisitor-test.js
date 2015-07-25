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
    const output = dedent(`
      # Plugin

      > Here is a block-quote. You should read it. # This is not considered a heading. And ${'```'} <-- that isn't a "pre fence"

      ## Code samples

      ${'```'}
      let g:example=1
      let g:other=2
      ${'```'}

      ### More stuff

      <a name="arbitrary-link-target"></a>
      We should probably <strong>g:LinkToOtherStuff</strong>.
    `).trim();
    expect(result.output).toEqual(output);
  });

  it('consults the supplied symbol table', () => {
    const input = dedent(`
      ""
      " @plugin Plugin Description of the plug-in
      "
      " A |g:LinkToOtherStuff| and |:augroup|.
    `);
    const ast = parse(lex(input));
    const visitor = new MarkdownVisitor(ast);

    // With no symbol table, we don't get links.
    let result = visitor.visit();
    let output = dedent(`
      # Plugin

      A <strong>g:LinkToOtherStuff</strong> and <strong>:augroup</strong>.
    `).trim();
    expect(result.output).toEqual(output);

    // With the symbol table, we get links for items in the table only.
    result = visitor.visit({
      table: {
        'g:LinkToOtherStuff': '[hash-g-linktootherstuff]',
      },
    });
    output = dedent(`
      # Plugin

      A <strong>[g:LinkToOtherStuff](#[hash-g-linktootherstuff])</strong> and <strong>:augroup</strong>.
    `).trim();
    expect(result.output).toEqual(output);
  });
});
