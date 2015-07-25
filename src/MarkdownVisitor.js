/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import Visitor from './Visitor';
import type {AST} from './parse';

type State = {
  output: string;
  table: Object;
};

export default class MarkdownVisitor<State> extends Visitor {
  getInitialState(): State {
    return {
      output: '',
      table: {},
    };
  }

  visitBlockQuoteNode(node: AST, state: State): ?AST {
    state.output += '> ';
    node.children.forEach(child => {
      state.output += child.content;
    });
    state.output = state.output.trim() + '\n\n';
    return null;
  }

  visitPreNode(node: AST, state: State): ?AST {
    state.output += '```\n';
    state.output += `${node.content}\n`;
    state.output += '```\n\n';
    return node;
  }

  visitHeadingNode(node: AST, state: State): ?AST {
    state.output += '# ' + node.content.trim() + '\n\n';
    return node;
  }

  visitSubHeadingNode(node: AST, state: State): ?AST {
    state.output += '## ' + node.content.trim() + '\n\n';
    return node;
  }

  visitLinkTargetNode(node: AST, state: State): ?AST {
    state.output += `<em><a name="${node.content}" />${node.content}</em>`;
    return node;
  }

  visitLinkNode(node: AST, state: State): ?AST {
    if (state.table.hasOwnProperty(node.content)) {
      state.output += `<strong>[${node.content}](#${state.table[node.content]})</strong>`;
    } else {
      state.output += `<strong>${node.content}</strong>`;
    }
    return node;
  }

  visitTextNode(node: AST, state: State): ?AST {
    state.output += node.content;
    return node;
  }
}
