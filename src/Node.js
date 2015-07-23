/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

const Node = {
  ANNOTATION: 'ANNOTATION',
  BLOCK_QUOTE: 'BLOCK_QUOTE',
  DOC_BLOCK: 'DOC_BLOCK',
  HEADING: 'HEADING',
  OPTION_ANNOTATION: 'OPTION_ANNOTATION',
  PLUGIN_ANNOTATION: 'PLUGIN_ANNOTATION',
  SUB_HEADING: 'SUB_HEADING',
  TEXT: 'TEXT',
  TRANSLATION_UNIT: 'TRANSLATION_UNIT',
};

export type NodeType = $Enum<typeof Node>; // eslint-disable-line no-undef
export default Node;
