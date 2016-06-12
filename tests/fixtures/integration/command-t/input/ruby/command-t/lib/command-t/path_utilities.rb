# Copyright 2010-present Greg Hurrell. All rights reserved.
# Licensed under the terms of the BSD 2-clause license.

module CommandT
  module PathUtilities

  private

    def relative_path_under_working_directory(path)
      # any path under the working directory will be specified as a relative
      # path to improve the readability of the buffer list etc
      pwd = File.expand_path(VIM::pwd) + '/'
      path.index(pwd) == 0 ? path[pwd.length..-1] : path
    end

  end
end
