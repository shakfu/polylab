#!/usr/bin/env ruby

=begin
A convenience script to update a pre-specified folder containing
subversion, mercurial, bazaar, and/or git source folders

To use it:
    - change the 'src' variable below to point to your source folder
    - name this script to something appropriate (I call it 'update')
    - put it into a directory on your PATH
=end

require 'pathname'

# define default source folder here
SRC = '/home/sa/src'

OPS = {
    '.bzr' => ['bzr pull', 'bzr update'],
    '.hg'  => ['hg pull', 'hg update'],
    '.svn' => ['svn update'],
    '.git' => ['git pull']
}


def update_sources(topdir)
    ops = OPS.keys.collect {|i| Pathname.new(i)}
    Dir[topdir + '/*'].each do |entry|
        dir = Pathname.new(entry) if File::directory? entry
        for t in dir.entries.collect {|i| i.basename}
            if ops.include? t
                Dir.chdir(dir)
                OPS[t.to_s].each do |cmd|
                    puts "\n"
                    puts "#{dir} --> #{cmd}"
                    system cmd
                end
            end
        end
    end
end

if $0 == __FILE__
    if ARGV.length >= 1
        ARGV.each { |dir| update_sources(dir) }
    else
        update_sources(SRC)
    end
end
