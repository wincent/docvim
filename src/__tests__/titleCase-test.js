/**
 * Copyright 2015-present Greg Hurrell. All rights reserved.
 * Licensed under the terms of the MIT license.
 */

'use strict';

import titleCase from '../titleCase';

describe('titleCase()', () => {
  it('converts to title case', () => {
    expect(titleCase('FOO_BAR')).toBe('FooBar');
  });
});
