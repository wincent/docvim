# Copyright 2010-present Greg Hurrell. All rights reserved.
# Licensed under the terms of the BSD 2-clause license.

module CommandT
  module VIM
    autoload :Screen, 'command-t/vim/screen'
    autoload :Window, 'command-t/vim/window'

    class << self
      # Check for the existence of a feature such as "conceal" or "syntax".
      def has?(feature)
        ::VIM::evaluate(%{has("#{feature}")}).to_i != 0
      end

      # Check for the presence of a setting such as:
      #
      #   - g:CommandTSmartCase (plug-in setting)
      #   - &wildignore         (Vim setting)
      #   - +cursorcolumn       (Vim setting, that works)
      #
      def exists?(str)
        ::VIM::evaluate(%{exists("#{str}")}).to_i != 0
      end

      def get_number(name)
        exists?(name) ? ::VIM::evaluate("#{name}").to_i : nil
      end

      def get_bool(name, default = nil)
        exists?(name) ? ::VIM::evaluate("#{name}").to_i != 0 : default
      end

      def get_string(name)
        exists?(name) ? ::VIM::evaluate("#{name}").to_s : nil
      end

      # expect a string or a list of strings
      def get_list_or_string(name)
        return nil unless exists?(name)
        list_or_string = ::VIM::evaluate("#{name}")
        if list_or_string.kind_of?(Array)
          list_or_string.map { |item| item.to_s }
        else
          list_or_string.to_s
        end
      end

      def pwd
        ::VIM::evaluate 'getcwd()'
      end

      def current_file_dir
        ::VIM::evaluate 'expand("%:p:h")'
      end

      # Execute cmd, capturing the output into a variable and returning it.
      def capture(cmd)
        ::VIM::command 'silent redir => g:command_t_captured_output'
        ::VIM::command cmd
        ::VIM::command 'silent redir END'
        ::VIM::evaluate 'g:command_t_captured_output'
      end

      # Escape a string for safe inclusion in a Vim single-quoted string
      # (single quotes escaped by doubling, everything else is literal)
      def escape_for_single_quotes(str)
        str.gsub "'", "''"
      end

      # Conservatively convert wildignore patterns that we understand to a
      # regex. Supported patterns noted in the inline comments below.
      #
      # If this ends up doing something wrong, set `g:CommandTWildIgnore` to ''
      # to opt out or otherwise override to produce a conforming pattern.
      def wildignore_to_regexp(str)
        patterns = str.split(',')
        regex = patterns.map do |pattern|
          if pattern.match(%r{\A([^*/]+)\z})
            # something (match file at any level)
            '(\A|/)' + Regexp.escape($~[1]) + '\z'
          elsif pattern.match(%r{\A\*\.([^*]+)\z})
            # *.something (match file with extension at any level)
            '\.' + Regexp.escape($~[1]) + '\z'
          elsif pattern.match(%r{\A\*/(.+)\z})
            # */something (match files or directories at any level)
            '(\A|/)' + Regexp.escape($~[1]) + '(/|\z)'
          elsif pattern.match(%r{\A\*/([^*]+)/*\z})
            # */something/* (match directories at any level)
            '(\A|/)' + Regexp.escape($~[1]) + '(/|\z)'
          end
        end.compact.join('|')
        Regexp.new(regex) unless regex.empty?
      end
    end
  end
end
