/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 *
 * @flow
 */

'use strict';

/**
 * Converts FOO_BAR to FooBar.
 */
export default function titleCase(input: string): string {
  return input.toLowerCase().replace(
    // Would use postive lookbehind here, but JS doesn't support that.
    /^.|_./g,
    match => match.toUpperCase().replace(/_(.)/, '$1')
  );
}
