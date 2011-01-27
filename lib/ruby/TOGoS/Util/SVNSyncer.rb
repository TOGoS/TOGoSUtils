require 'fileutils'
require 'TOGoS/Util/DirWalker'
require 'TOGoS/Util/ProcUtil'

module TOGoS ; module Util
  class SVNSyncer
    include ProcUtil
    
    def sync( src, dest )
      src  = DirWalker.strip_slash( src )
      dest = DirWalker.strip_slash( dest )
      
      add_ppaths    = []
      adddir_ppaths = []
      update_ppaths = []
      rm_ppaths     = []
      rmdir_ppaths  = []
      
      DirWalker.walk( src, true ) do |srcpath|
        next if srcpath == src
        ppath = DirWalker.postfix( src+'/', srcpath )
        destpath = "#{dest}/#{ppath}"
        if File.directory? srcpath
          unless File.exist? destpath
            adddir_ppaths << ppath
          end
        else
          if File.exist? destpath
            update_ppaths << ppath
          else
            add_ppaths << ppath
          end
        end
      end
      
      DirWalker.walk( dest, true ) do |destpath|
        next if destpath == dest
        ppath = DirWalker.postfix( dest+'/', destpath )
        srcpath = "#{src}/#{ppath}"
        if File.directory? destpath
          unless File.exist? srcpath
            rmdir_ppaths << ppath
          end
        else
          unless File.exist? srcpath
            rm_ppaths << ppath
          end
        end
      end
      
      fu = FileUtils
      for ppath in adddir_ppaths
        fu.mkdir_p( "#{dest}/#{ppath}" )
      end
      for ppath in add_ppaths
        fu.cp( "#{src}/#{ppath}", "#{dest}/#{ppath}" )
      end
      for ppath in update_ppaths
        fu.cp( "#{src}/#{ppath}", "#{dest}/#{ppath}" )
      end
      
      Dir.chdir( dest ) {
        for ppath in adddir_ppaths
           sys('svn','add',ppath)
        end
        for ppath in add_ppaths
           sys('svn','add',ppath)
        end
        for ppath in rm_ppaths
          sys('svn','rm',ppath)
        end
        for ppath in rmdir_ppaths.reverse
          sys('svn','rm',ppath)
        end
      }
    end
  end
end ; end
