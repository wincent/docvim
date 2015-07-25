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
  WORD,
} from './Token';
import type {Token} from './lex';

export type AST = {
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

  /**
   * Rolls back the last token.
   */
  _rollbackToken(): void {
    console.assert(
      this._currentTokenIndex,
      'Cannot rollback last token.'
    );
    this._currentTokenIndex--;
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
        result.option = this._getNextToken(WORD).content.trim();
        result.type = this._getNextToken(WORD).content.trim();
        result.default = this._getNextToken(WORD).content.trim();
        this._skipNextToken(NEW_LINE);
        result.children = this._parseMarkdown();
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
        this._skipNextToken(NEW_LINE);
        result.children = this._parseMarkdown();
        break;
      default:
        throw new Error(`Unrecognized annotation type: ${annotation.type}`);
    }
    return result;
  }

  _parseMarkdown(): Array<AST> {
    const result = [];
    for (;;) {
      const token = this._getNextToken();
      if (!token) {
        break;
      }
      switch (token.type) {
        case ANNOTATION:
        case DOC_BLOCK_START:
        case NON_COMMENT_LINE:
          this._rollbackToken();
          return result;
        case COMMENT_START:
        case NEW_LINE:
          break;
        case BLOCK_QUOTE:
          result.push(this._parseBlockQuote());
          break;
        case HEADING:
          result.push(this._parseHeading(Node.HEADING));
          break;
        case SUB_HEADING:
          result.push(this._parseHeading(Node.SUB_HEADING));
          break;
        case PRE_FENCE:
          this._getNextToken(NEW_LINE);
          result.push(this._parsePre(Node.PRE));
          break;
        case LINK_TARGET:
          result.push({
            content: token.content.replace(/^\*|\*\s*$/g, ''),
            name: Node.LINK_TARGET,
          });
          break;
        case LINK:
          {
            result.push({
              content: token.content.replace(/^\||\|\s*$/g, ''),
              name: Node.LINK,
            });
            const match = token.content.match(/(\s+)$/);
            if (match) {
              result.push({
                content: match[1],
                name: Node.TEXT,
              });
            }
          }
          break;
        case WORD:
          {
            // Merge consecutive WORD children.
            // Replace newlines with a space.
            const previous = last(result);
            const content = token.content.replace(/\n/, ' ');
            if (previous && previous.name === Node.TEXT) {
              previous.content += content;
            } else {
              result.push({
                content,
                name: Node.TEXT,
              });
            }
          }
          break;
      }
    }
    return result;
  }

  _parseBlockQuote(): AST {
    const result = {
      name: Node.BLOCK_QUOTE,
      children: [],
    };
    for (;;) {
      const token = this._getNextToken();
      if (!token) {
        break;
      }
      switch (token.type) {
        case ANNOTATION:
        case DOC_BLOCK_START:
        case NON_COMMENT_LINE:
          this._rollbackToken();
          return result;
        case BLOCK_QUOTE:
          break;
        case COMMENT_START:
          if (!this._peekNextToken(BLOCK_QUOTE)) {
            return result;
          }
          break;
        case NEW_LINE:
        case WORD:
          {
            // Merge consecutive WORD children.
            // Replace newlines with a space.
            const previous = last(result.children);
            const content = token.content.replace(/\n/, ' ');
            if (previous && previous.name === Node.TEXT) {
              previous.content += content;
            } else {
              result.children.push({
                name: Node.TEXT,
                content,
              });
            }
          }
          break;
      }
    }
    return result;
  }

  _parsePre(): AST {
    const result = {
      name: Node.PRE,
      content: '',
    };
    for (;;) {
      const token = this._getNextToken();
      if (!token) {
        break;
      }
      switch (token.type) {
        case PRE_FENCE:
          // End of the block.
          this._getNextToken(NEW_LINE);
          result.content = result.content.trim();
          return result;
        case COMMENT_START:
          break;
        case NON_COMMENT_LINE:
          throw new Error(`Found unterminated PRE block at ${token.position}`);
        default:
          result.content += token.content;
      }
    }
    return result;
  }

  _parseHeading(nodeType: string): AST {
    const content = [];
    for (;;) {
      const token = this._getNextToken();
      if (!token || token.type === NEW_LINE) {
        break;
      } else if (token.type !== WORD) {
        this._rollbackToken();
        break;
      }
      // Merge consecutive WORD children.
      content.push(token.content.trim());
    }
    return {
      name: nodeType,
      content: content.join(' '),
    };
  }
}

/**
 * Convenience helper to get the last item in an array.
 */
function last(array: Array): mixed {
  if (array.length) {
    return array[array.length - 1];
  } else {
    return null;
  }
}

export default function parse(input: Array<Token>): AST {
  const parser = new Parser(input);
  return parser.parse();
}
