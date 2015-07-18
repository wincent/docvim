/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Node from './Node';
import {
  ANNOTATION,
  COMMENT_START,
  DOC_BLOCK_START,
  NEW_LINE,
  NON_COMMENT_LINE,
  WORD,
} from './Token';
import type {Token} from './lex';

type AST = {
  children?: Array<AST>;
  content?: string;
  name: string;
  [key: string]: mixed;
};

class Parser {
  _currentTokenIndex: number;
  _input: Array<Token>;

  constructor(input: Array<Token>) {
    this._currentTokenIndex = 0;
    this._input = input;
  }

  parse(): AST {
    return {
      children: this._parseTranslationUnit(),
      name: Node.TRANSLATION_UNIT,
    };
  }

  /**
   * Gets the next token in the input, or `null` if there is no next token.
   *
   * If `type` is supplied and the next token does not match the type, an error
   * is raised.
   */
  _getNextToken(type: ?string): ?Token {
    const token = this._input[this._currentTokenIndex++] || null;
    if (
      type &&
      (!token || token.type !== type)
    ) {
      const position = token ? token.position : 'EOF';
      const actual = token ? token.type : 'NULL';
      throw new Error(
        `Expected token of type ${type} at position ${position} ` +
        `but got ${actual}.`
      );
    }
    return token;
  }

  /**
   * Peek ahead and return the next token in the input, or `null` if there is no
   * next token.
   *
   * If `type` is supplied and the next token does not match the type, `null` is
   * returned.
   */
  _peekNextToken(type: ?string): ?Token {
    const token = this._input[this._currentTokenIndex];
    if (!token) {
      return null;
    } else if (type && token.type !== type) {
      return null;
    }
    return token;
  }

  /**
   * Skips over the next token in the input, but only if it is of type `type`.
   */
  _skipNextToken(type: string): void {
    if (this._peekNextToken(type)) {
      this._getNextToken();
    }
  }

  _parseTranslationUnit(): Array<AST> {
    const result = [];
    for (;;) {
      const token = this._getNextToken();
      if (!token) {
        break;
      } else if (token.type === DOC_BLOCK_START) {
        result.push(this._parseDocBlock());
      }
    }
    return result;
  }

  _parseDocBlock(): AST {
    const result = {
      children: [],
      name: Node.DOC_BLOCK,
    };
    for (;;) {
      const token = this._getNextToken();
      if (!token) {
        break;
      }
      switch (token.type) {
        case COMMENT_START:
          break;
        case ANNOTATION:
          result.children.push(this._parseAnnotation(token));
          break;

        case DOC_BLOCK_START:
          // Technically, this should be an error.
          // TODO: print a warning.
          return result;
        case NON_COMMENT_LINE:
          return result;
      }
    }
    return result;
  }

  _parseAnnotation(annotation: Token): AST {
    const result = {};
    switch (annotation.content.trim()) {
      case '@option':
        result.name = Node.OPTION_ANNOTATION;
        break;
      case '@plugin':
        result.name = Node.PLUGIN_ANNOTATION;
        result.plugin = this._getNextToken(WORD).content.trim();
        result.description = '';
        for (;;) {
          if (this._peekNextToken(WORD)) {
            result.description += this._getNextToken().content;
          } else {
            break;
          }
        }
        result.children = this._parsePluginAnnotation();
        break;
      default:
        throw new Error(`Unrecognized annotation type: ${annotation.type}`);
    }
    return result;
  }

  _parsePluginAnnotation(): AST {
    this._skipNextToken(NEW_LINE);
    return [];
  }

}

export default function parse(input: Array<Token>): AST {
  const parser = new Parser(input);
  return parser.parse();
}