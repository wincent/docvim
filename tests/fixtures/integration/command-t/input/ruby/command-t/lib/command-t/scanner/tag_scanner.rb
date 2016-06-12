# Copyright 2011-present Greg Hurrell. All rights reserved.
# Licensed under the terms of the BSD 2-clause license.

module CommandT
  class Scanner
    class TagScanner < Scanner
      attr_reader :include_filenames

      def initialize(options = {})
        @include_filenames = options[:include_filenames] || false
        @cached_tags = nil
      end

      def paths
        @cached_tags ||= taglist.map do |tag|
          path = tag['name']
          path << ":#{tag['filename']}" if @include_filenames
          path
        end.uniq.sort
      end

      def flush
        @cached_tags = nil
      end

    private

      def taglist
        ::VIM::evaluate 'taglist(".")'
      end
    end
  end
end
