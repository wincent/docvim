/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import {
  ANNOTATION,
  BLOCK_QUOTE,
  COMMENT_START,
  DOC_BLOCK_START,
  HEADING,
  LINK,
  LINK_TARGET,
  NEW_LINE,
  NON_COMMENT_LINE,
  PRE_FENCE,
  SUB_HEADING,
  WORD
} from './Token';

type Token = {
  content: string;
  position: number;
  // TODO: make this a proper $Enum reference to Token
  type: string;
};

/**
 * Dead-simple lexer.
 *
 * Not intended to be ultra efficient, but rather, provide an easy framework for
 * throwing regexp-based token recognizers together.
 */
export default function lex(input: string): Array<Token> {
  let position = 0;
  let remaining = input;
  const tokens = [];

  function lastToken(type: ?string) {
    if (tokens.length) {
      const token = tokens[tokens.length - 1];
      if (type) {
        return token.type === type ? token : null;
      } else {
        return token;
      }
    }
    return null;
  }

  /**
   * Checks whether the input string has the token of type `type` at the current
   * position by testing it with the supplied `regexp`. The `success` callback
   * is called on a successful match, and has the opportunity to modify the
   * token (by mutating it), or suppress it (by returning `null`)
   */
  function check(
    type: string,
    regexp: RegExp,
    success: (match: Object, token: Token) => ?Token
  ): boolean {
    if (regexp.source.startsWith('^')) {
      // Require that match be at start of a line.
      if (position && input[position - 1] !== '\n') {
        return false;
      }
    } else {
      // Force search to be anchored.
      regexp = new RegExp('^' + regexp.source);
    }
    const match = remaining.match(regexp);
    if (match && match[0].length) {
      const length = match[0].length;
      const token = {
        content: match[0],
        position,
        type,
      };
      const result = success && success(match, token);
      if (result === false) {
        return false;
      } else if (result !== null) {
        tokens.push(token);
      }
      position += length;
      remaining = remaining.slice(length);
      return true;
    }
    return false;
  }

  const onlyAfterCommentStart = (_, token) => {
    if (!lastToken(COMMENT_START)) {
      token.type = WORD;
    }
  };

  while (remaining.length) {
    const rules = [
      // TODO: use/abuse sweet.js to make a DSL?
      () => check(NON_COMMENT_LINE, /^[ \t]*[^"\n]*?($|\n)/, () => null),
      () => check(DOC_BLOCK_START, /^[ \t]*""[ \t]*($|\n)/),
      () => check(COMMENT_START, /^[ \t]*"[ \t]*/),
      () => check(HEADING, /#[ \t]*/, onlyAfterCommentStart),
      () => check(SUB_HEADING, /##[ \t]*/, onlyAfterCommentStart),
      () => check(ANNOTATION, /@[a-z]+[ \t]*/, onlyAfterCommentStart),
      () => check(LINK, /\|[^| \t\n]\|/),
      () => check(LINK_TARGET, /\*[^* \t\n]\*/),
      () => check(BLOCK_QUOTE, />[ \t]*/, onlyAfterCommentStart),
      () => check(PRE_FENCE, /```[ \t]*/, onlyAfterCommentStart),
      () => check(WORD, /\S+[ \t]*/),
      () => check(NEW_LINE, /\n/),
    ];

    if (!rules.find(rule => rule())) {
      break;
    }
  }

  if (remaining.length) {
    throw new Error(
      `Failed to consume input string: at position ${position}, ` +
      `remaining unprocessed length ${remaining.length}`
    );
  }

  return tokens;
};
