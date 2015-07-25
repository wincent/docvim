/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import type {AST} from './parse';
import titleCase from './titleCase';

export default class Visitor<Ts: Object> {
  _ast: AST;

  constructor(ast: AST) {
    this._ast = ast;
  }

  getInitialState(): Ts {
    // Generic default, subclasses override.
    return {};
  }

  /**
   * Generic entry point, starts visiting the AST from the root.
   */
  visit(state: ?Ts): Ts {
    if (state) {
      state = {...this.getInitialState(), ...state};
    } else {
      state = this.getInitialState();
    }
    this.visitNode(this._ast, state);
    return state;
  }

  /**
   * Traverse `node`'s children, if they exist.
   */
  traverse(node: AST, state: Ts): ?AST {
    if (node.children) {
      node.children.forEach(child => this.visitNode(child, state));
    }
    return node;
  }

  /**
   * Generic node visitor.
   */
  visitNode(node: AST, state: Ts): ?AST {
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
