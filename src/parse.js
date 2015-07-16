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
  NON_COMMENT_LINE,
} from './Token';
import type {Token} from './lex';

type AST = {
  children?: Array<AST>;
  content?: string;
  name: string;
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

  _getNextToken(): ?Token {
    return this._input[this._currentTokenIndex++] || null;
  }

  _peekNextToken(): ?Token {
    return this._input[this._currentTokenIndex + 1] || null;
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
    let name;
    switch (annotation.content.trim()) {
      case '@option':
        name = Node.OPTION_ANNOTATION;
        break;
      case '@plugin':
        name = Node.PLUGIN_ANNOTATION;
        break;
      default:
        throw new Error(`Unrecognized annotation type: ${annotation.type}`);
    }
    const result = {
      name,
      children: [],
    };
    // TODO: store WORD tokens as children until newline
    return result;
  }
}

export default function parse(input: Array<Token>): AST {
  const parser = new Parser(input);
  return parser.parse();
}
