/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

import dedent from '../dedent';
import parse from '../parse';
import lex from '../lex';

describe('parse()', () => {
  it('parses an empty file', () => {
    expect(parse(lex(''))).toEqual({
      children: [],
      name: 'TRANSLATION_UNIT',
    });
  });

  it('parses a file with a minimal doc block', () => {
    expect(parse(lex('""'))).toEqual({
      children: [{
        children: [],
        name: 'DOC_BLOCK',
      }],
      name: 'TRANSLATION_UNIT',
    });
  });

  it('parses a file with a slightly less minimal doc block', () => {
    const input = dedent(`
      ""
      "
    `);
    expect(parse(lex(input))).toEqual({
      children: [{
        children: [],
        name: 'DOC_BLOCK',
      }],
      name: 'TRANSLATION_UNIT',
    });
  });

  it('parses a file with a complex doc block', () => {
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
    expect(parse(lex(input))).toEqual({
      children: [{
        children: [
          {
            children: [{
              children: [{
                content: 'Here is a block-quote. You should read it. # This is not considered a heading. And ``` <-- that isn\'t a "pre fence"',
                name: 'TEXT',
              }],
              name: 'BLOCK_QUOTE',
            },
            {
              content: 'Code samples',
              name: 'HEADING',
            },
            {
              content:
                'let g:example=1\n' +
                'let g:other=2',
              name: 'PRE',
            },
            {
              content: 'More stuff',
              name: 'SUB_HEADING',
            },
            {
              content: 'arbitrary-link-target',
              name: 'LINK_TARGET',
            },
            {
              content: 'We should probably',
              name: 'TEXT',
            },
            {
              content: 'g:LinkToOtherStuff',
              name: 'LINK',
            },
            {
              content: '.',
              name: 'TEXT',
            },
          ],
          description: 'Description of the plug-in',
          name: 'PLUGIN_ANNOTATION',
          plugin: 'Plugin',
        }],
        name: 'DOC_BLOCK',
      }],
      name: 'TRANSLATION_UNIT',
    });
  });
});
