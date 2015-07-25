/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import type {VisitorState} from './Visitor';
import Visitor from './Visitor';

export default class MarkdownVisitor extends Visitor {
  visitDocBlockNode(node: AST, state: VisitorState): ?AST {
    return node;
  }

  visitBlockQuoteNode(node: AST, state: VisitorState): ?AST {
    state.output += '> ';
    node.children.forEach(child => {
      state.output += child.content;
    });
    state.output = state.output.trim() + '\n\n';
    return null;
  }

  visitPreNode(node: AST, state: VisitorState): ?AST {
    state.output += '```\n';
    state.output += `${node.content}\n`;
    state.output += '```\n\n';
    return node;
  }

  visitHeadingNode(node: AST, state: VisitorState): ?AST {
    state.output += '# ' + node.content.trim() + '\n\n';
    return node;
  }

  visitSubHeadingNode(node: AST, state: VisitorState): ?AST {
    state.output += '## ' + node.content.trim() + '\n\n';
    return node;
  }

  visitLinkTargetNode(node: AST, state: VisitorState): ?AST {
    state.output += `<em><a name="${node.content}" />${node.content}</em>`;
    return node;
  }

  visitLinkNode(node: AST, state: VisitorState): ?AST {
    state.output += `<strong>[${node.content}](#${node.content})</strong>`;
    return node;
  }

  visitTextNode(node: AST, state: VisitorState): ?AST {
    state.output += node.content;
    return node;
  }
}
