/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

const Token = {
  ANNOTATION: 'ANNOTATION',
  BLOCK_QUOTE: 'BLOCK_QUOTE',
  COMMENT_START: 'COMMENT_START',
  DOC_BLOCK_START: 'DOC_BLOCK_START',
  HEADING: 'HEADING',
  LINK: 'LINK',
  LINK_TARGET: 'TARGET',
  NEW_LINE: 'NEW_LINE',
  NON_COMMENT_LINE: 'NON_COMMENT_LINE',
  PRE_FENCE: 'PRE_FENCE',
  SUB_HEADING: 'SUB_HEADING',
  WORD: 'WORD',
  // TODO: images, markdown links?
};

export default Token;
