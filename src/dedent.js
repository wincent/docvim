/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

/**
 * Utility function for turning pretty multi-line template strings (ie. ones
 * indented to match their surroundings) into their dedented form.
 */
export default function dedent(string) {
  // Get rid of leading and trailing newlines.
  string = string.replace(/^\n|\n$/g, '');

  // Determine indent based on first line.
  const match = string.match(/^ */);
  const amount = match ? match[0].length : 0;

  // Dedent.
  return string.replace(
    new RegExp('^ {' + amount + '}', 'gm'),
    ''
  );
}
