# Copyright 2013-present Greg Hurrell. All rights reserved.
# Licensed under the terms of the BSD 2-clause license.

require 'rbconfig'

module CommandT
  module Util
    class << self
      def processor_count
        @processor_count ||= begin
          count = processor_count!
          count = 1 if count < 1   # sanity check
          count = 32 if count > 32 # sanity check
          count
        end
      end

    private

      # This method derived from:
      #
      #   https://github.com/grosser/parallel/blob/d11e4a3c8c1a/lib/parallel.rb
      #
      # Number of processors seen by the OS and used for process scheduling.
      #
      # * AIX: /usr/sbin/pmcycles (AIX 5+), /usr/sbin/lsdev
      # * BSD: /sbin/sysctl
      # * Cygwin: /proc/cpuinfo
      # * Darwin: /usr/bin/hwprefs, /usr/sbin/sysctl
      # * HP-UX: /usr/sbin/ioscan
      # * IRIX: /usr/sbin/sysconf
      # * Linux: /proc/cpuinfo
      # * Minix 3+: /proc/cpuinfo
      # * Solaris: /usr/sbin/psrinfo
      # * Tru64 UNIX: /usr/sbin/psrinfo
      # * UnixWare: /usr/sbin/psrinfo
      #
      # Copyright (C) 2013 Michael Grosser <michael@grosser.it>
      #
      # Permission is hereby granted, free of charge, to any person obtaining
      # a copy of this software and associated documentation files (the
      # "Software"), to deal in the Software without restriction, including
      # without limitation the rights to use, copy, modify, merge, publish,
      # distribute, sublicense, and/or sell copies of the Software, and to
      # permit persons to whom the Software is furnished to do so, subject to
      # the following conditions:
      #
      # The above copyright notice and this permission notice shall be
      # included in all copies or substantial portions of the Software.
      #
      # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
      # EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
      # MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
      # NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
      # LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
      # OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
      # WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
      #
      def processor_count!
        os_name = RbConfig::CONFIG['target_os']
        if os_name =~ /mingw|mswin/
          require 'win32ole'
          result = WIN32OLE.connect('winmgmts://').ExecQuery(
              'select NumberOfLogicalProcessors from Win32_Processor')
          result.to_enum.collect(&:NumberOfLogicalProcessors).reduce(:+)
        elsif File.readable?('/proc/cpuinfo')
          IO.read('/proc/cpuinfo').scan(/^processor/).size
        elsif File.executable?('/usr/bin/hwprefs')
          IO.popen(%w[/usr/bin/hwprefs thread_count]).read.to_i
        elsif File.executable?('/usr/sbin/psrinfo')
          IO.popen('/usr/sbin/psrinfo').read.scan(/^.*on-*line/).size
        elsif File.executable?('/usr/sbin/ioscan')
          IO.popen(%w[/usr/sbin/ioscan -kC processor]) do |out|
            out.read.scan(/^.*processor/).size
          end
        elsif File.executable?('/usr/sbin/pmcycles')
          IO.popen(%w[/usr/sbin/pmcycles -m]).read.count("\n")
        elsif File.executable?('/usr/sbin/lsdev')
          IO.popen(%w[/usr/sbin/lsdev -Cc processor -S 1]).read.count("\n")
        elsif File.executable?('/usr/sbin/sysconf') && os_name =~ /irix/i
          IO.popen(%w[/usr/sbin/sysconf NPROC_ONLN]).read.to_i
        elsif File.executable?('/usr/sbin/sysctl')
          IO.popen(%w[/usr/sbin/sysctl -n hw.ncpu]).read.to_i
        elsif File.executable?('/sbin/sysctl')
          IO.popen(%w[/sbin/sysctl -n hw.ncpu]).read.to_i
        else # unknown platform
          1
        end
      rescue
        1
      end
    end
  end
end
