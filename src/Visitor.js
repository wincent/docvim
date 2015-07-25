/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import titleCase from './titleCase';

export type VisitorState = {
  output: string;
  [key: string]: mixed;
};

export default class Visitor {
  _ast: AST;

  constructor(ast: AST) {
    this._ast = ast;
  }

  /**
   * Generic entry point, starts visiting the AST from the root.
   */
  visit(): mixed {
    const state = {output: ''};
    this.visitNode(this._ast, state);
    return state.output;
  }

  /**
   * Traverse `node`'s children, if they exist.
   */
  traverse(node: AST, state: VisitorState): ?AST {
    if (node.children) {
      node.children.forEach(child => this.visitNode(child, state));
    }
    return node;
  }

  /**
   * Generic node visitor.
   */
  visitNode(node: AST, state: VisitorState): ?AST {
    // Call specialized visit method, if available.
    const specialized = 'visit' + titleCase(node.name) + 'Node';
    if (specialized in this) {
      const transformed = this[specialized](node, state);
      if (transformed) {
        return this.traverse(transformed, state);
      } else {
        return null;
      }
    } else {
      return this.traverse(node, state);
    }
  }
}
