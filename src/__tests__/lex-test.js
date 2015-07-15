/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

const lex = require('../lex');

/**
 * Utility function for turning pretty multi-line template strings (ie. ones
 * indented to match their surroundings) into their dedented form.
 */
function dedent(string) {
  // Get rid of leading and trailing newlines.
  string = string.replace(/^\n|\n$/g, '');

  // Determine indent based on first line.
  const match = string.match(/^ */);
  const amount = match ? match[0].length : 0;

  // Dedent.
  return string.replace(
    new RegExp('^ {' + amount + '}', 'gm'),
    ''
  );
}

describe('lex()', () => {
  it('lexes an empty string', () => {
    expect(lex('')).toEqual([]);
  });

  it('skips non-comment lines', () => {
    expect(lex('something\n' + 'something else')).toEqual([]);
  });

  it('recognizes a doc block start', () => {
    expect(lex('""')).toEqual([
      {content: '""', position: 0, type: 'DOC_BLOCK_START'},
    ]);

    // With leading whitespace.
    expect(lex('   ""')).toEqual([
      {content: '   ""', position: 0, type: 'DOC_BLOCK_START'},
    ]);

    // With trailing whitespace.
    expect(lex('""    ')).toEqual([
      {content: '""    ', position: 0, type: 'DOC_BLOCK_START'},
    ]);

    // With leading and trailing whitespace.
    expect(lex('    ""    ')).toEqual([
      {content: '    ""    ', position: 0, type: 'DOC_BLOCK_START'},
    ]);

    // With tabs.
    expect(lex(' \t   ""  \t  ')).toEqual([
      {content: ' \t   ""  \t  ', position: 0, type: 'DOC_BLOCK_START'},
    ]);
  });

  it('recognizes a comment start', () => {
    expect(lex('"')).toEqual([
      {content: '"', position: 0, type: 'COMMENT_START'},
    ]);

    // Leading whitespace.
    expect(lex(' \t"')).toEqual([
      {content: ' \t"', position: 0, type: 'COMMENT_START'},
    ]);

    // Trailing whitespace.
    expect(lex('"\t ')).toEqual([
      {content: '"\t ', position: 0, type: 'COMMENT_START'},
    ]);
  });

  it('recognizes multiple comment starts', () => {
    expect(lex('"   \n' + '\t"\n')).toEqual([
      {content: '"   ', position: 0, type: 'COMMENT_START'},
      {content: '\n', position: 4, type: 'NEW_LINE'},
      {content: '\t"', position: 5, type: 'COMMENT_START'},
      {content: '\n', position: 7, type: 'NEW_LINE'},
    ]);
  });

  it('recognizes simple combinations of tokens', () => {
    expect(lex('""\n' + '"\n' + 'something')).toEqual([
      {content: '""\n', position: 0, type: 'DOC_BLOCK_START' },
      {content: '"', position: 3, type: 'COMMENT_START'},
      {content: '\n', position: 4, type: 'NEW_LINE'},
    ]);
  });

  it('recognizes complex combinations of tokens', () => {
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
    expect(lex(input)).toEqual([
      {content: '""\n', position: 15, type: 'DOC_BLOCK_START'},
      {content: '" ', position: 18, type: 'COMMENT_START'},
      {content: '@plugin ', position: 20, type: 'ANNOTATION'},
      {content: 'Plugin ', position: 28, type: 'WORD'},
      {content: 'Description ', position: 35, type: 'WORD'},
      {content: 'of ', position: 47, type: 'WORD'},
      {content: 'the ', position: 50, type: 'WORD'},
      {content: 'plug-in', position: 54, type: 'WORD'},
      {content: '\n', position: 61, type: 'NEW_LINE'},
      {content: '"', position: 62, type: 'COMMENT_START'},
      {content: '\n', position: 63, type: 'NEW_LINE'},
      {content: '" ', position: 64, type: 'COMMENT_START'},
      {content: '> ', position: 66, type: 'BLOCK_QUOTE'},
      {content: 'Here ', position: 68, type: 'WORD'},
      {content: 'is ', position: 73, type: 'WORD'},
      {content: 'a ', position: 76, type: 'WORD'},
      {content: 'block-quote.', position: 78, type: 'WORD'},
      {content: '\n', position: 90, type: 'NEW_LINE'},
      {content: '" ', position: 91, type: 'COMMENT_START'},
      {content: '> ', position: 93, type: 'BLOCK_QUOTE'},
      {content: 'You ', position: 95, type: 'WORD'},
      {content: 'should ', position: 99, type: 'WORD'},
      {content: 'read ', position: 106, type: 'WORD'},
      {content: 'it.', position: 111, type: 'WORD'},
      {content: '\n', position: 114, type: 'NEW_LINE'},
      {content: '" ', position: 115, type: 'COMMENT_START'},
      {content: '> ', position: 117, type: 'BLOCK_QUOTE'},
      {content: '# ', position: 119, type: 'WORD'},
      {content: 'This ', position: 121, type: 'WORD'},
      {content: 'is ', position: 126, type: 'WORD'},
      {content: 'not ', position: 129, type: 'WORD'},
      {content: 'considered ', position: 133, type: 'WORD'},
      {content: 'a ', position: 144, type: 'WORD'},
      {content: 'heading.', position: 146, type: 'WORD'},
      {content: '\n', position: 154, type: 'NEW_LINE'},
      {content: '" ', position: 155, type: 'COMMENT_START'},
      {content: '> ', position: 157, type: 'BLOCK_QUOTE'},
      {content: 'And ', position: 159, type: 'WORD'},
      {content: '``` ', position: 163, type: 'WORD'},
      {content: '<-- ', position: 167, type: 'WORD'},
      {content: 'that ', position: 171, type: 'WORD'},
      {content: 'isn\'t ', position: 176, type: 'WORD'},
      {content: 'a ', position: 182, type: 'WORD'},
      {content: '"pre ', position: 184, type: 'WORD'},
      {content: 'fence"', position: 189, type: 'WORD'},
      {content: '\n', position: 195, type: 'NEW_LINE'},
      {content: '"', position: 196, type: 'COMMENT_START'},
      {content: '\n', position: 197, type: 'NEW_LINE'},
      {content: '" ', position: 198, type: 'COMMENT_START'},
      {content: '# ', position: 200, type: 'HEADING'},
      {content: 'Code ', position: 202, type: 'WORD'},
      {content: 'samples', position: 207, type: 'WORD'},
      {content: '\n', position: 214, type: 'NEW_LINE'},
      {content: '"', position: 215, type: 'COMMENT_START'},
      {content: '\n', position: 216, type: 'NEW_LINE'},
      {content: '" ', position: 217, type: 'COMMENT_START'},
      {content: '```', position: 219, type: 'PRE_FENCE'},
      {content: '\n', position: 222, type: 'NEW_LINE'},
      {content: '" ', position: 223, type: 'COMMENT_START'},
      {content: 'let ', position: 225, type: 'WORD'},
      {content: 'g:example=1', position: 229, type: 'WORD'},
      {content: '\n', position: 240, type: 'NEW_LINE'},
      {content: '" ', position: 241, type: 'COMMENT_START'},
      {content: 'let ', position: 243, type: 'WORD'},
      {content: 'g:other=2', position: 247, type: 'WORD'},
      {content: '\n', position: 256, type: 'NEW_LINE'},
      {content: '" ', position: 257, type: 'COMMENT_START'},
      {content: '```', position: 259, type: 'PRE_FENCE'},
      {content: '\n', position: 262, type: 'NEW_LINE'},
      {content: '"', position: 263, type: 'COMMENT_START'},
      {content: '\n', position: 264, type: 'NEW_LINE'},
      {content: '" ', position: 265, type: 'COMMENT_START'},
      {content: '#', position: 267, type: 'HEADING'},
      {content: '# ', position: 268, type: 'WORD'},
      {content: 'More ', position: 270, type: 'WORD'},
      {content: 'stuff', position: 275, type: 'WORD'},
      {content: '\n', position: 280, type: 'NEW_LINE'},
      {content: '"', position: 281, type: 'COMMENT_START'},
      {content: '\n', position: 282, type: 'NEW_LINE'},
      {content: '" ', position: 283, type: 'COMMENT_START'},
      {content: '*arbitrary-link-target*', position: 285, type: 'WORD'},
      {content: '\n', position: 308, type: 'NEW_LINE'},
      {content: '" ', position: 309, type: 'COMMENT_START'},
      {content: 'We ', position: 311, type: 'WORD'},
      {content: 'should ', position: 314, type: 'WORD'},
      {content: 'probably ', position: 321, type: 'WORD'},
      {content: '|g:LinkToOtherStuff|.', position: 330, type: 'WORD'},
      {content: '\n', position: 351, type: 'NEW_LINE'}
    ]);
  });
});
