/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Visitor from './Visitor';
import {fingerprint32} from 'farmhash';
import type {AST} from './parse';

type State = {
  table: {
    [key: string]: string;
  };
};

/**
 * Produces a stable, sanitized anchor target for the passed in `target`.
 */
function anchor(target: string) {
  const hash = fingerprint32(target);
  const humanReadablePortion = target
    .toLowerCase()
    .replace(/[^a-z]+/g, '-');
  return [hash, humanReadablePortion]
    .join('-')
    .replace(/-+/g, '-')
    .replace(/^-|-$/g, '');
}

/**
 * Traverses entire AST building up symbol table representing linkable targets.
 */
export default class SymbolVisitor<State> extends Visitor {
  getInitialState(): State {
    return {
      table: {},
    }
  }

  visitLinkTargetNode(node: AST, state: State) {
    if (state.table[node.content]) {
      throw new Error('Duplicate link target `' + node.content + '`');
    }
    state.table[node.content] = anchor(node.content);
    return node;
  }
}
