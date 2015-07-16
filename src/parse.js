/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

import type {Token} from './lex';

// TODO: better type here
type AST = Object;

export default function parse(input: Array<Token>): AST {
  for (var i = 0; i < input.length; i++) {
    const token = input[i]; // eslint-disable-line no-unused-vars
  }
  return {};
}
