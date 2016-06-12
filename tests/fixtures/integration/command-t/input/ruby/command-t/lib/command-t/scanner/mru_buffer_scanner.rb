# Copyright 2014-present Greg Hurrell. All rights reserved.
# Licensed under the terms of the BSD 2-clause license.

module CommandT
  class Scanner
    # Returns a list of all open buffers, sorted in MRU order.
    class MRUBufferScanner < BufferScanner
      include PathUtilities

      def paths
        # Collect all buffers that have not been used yet.
        unused_buffers = (0..(::VIM::Buffer.count - 1)).map do |n|
          buffer = ::VIM::Buffer[n]
          buffer if buffer.name && !MRU.used?(buffer)
        end

        # Combine all most recently used buffers and all unused buffers, and
        # return all listed buffer paths.
        (unused_buffers + MRU.stack).map do |buffer|
          if buffer && buffer.name
            relative_path_under_working_directory buffer.name
          end
        end.compact.reverse
      end
    end
  end
end
