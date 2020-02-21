module TOGoS ; module Util
  module DirWalker
    def self.strip_slash( path )
      if path[-1] == ?/
        return path[0..-2]
      else
        return path
      end
    end
    
    def self.postfix( prefix, path )
      if path[0..(prefix.length-1)] == prefix
        return path[(prefix.length)..-1]
      else
        raise "#{path} does not start with #{prefix}!"
      end
    end
    
    def self.walk( dir, includedirs=false, &prok )
      if File.directory?( dir )
        prok.call( dir ) if includedirs
        Dir.foreach( dir ) do |fn|
          next if fn[0] == ?.
          walk( "#{dir}/#{fn}", includedirs, &prok )
        end
      else
        prok.call( dir )
      end
    end
  end
end ; end
