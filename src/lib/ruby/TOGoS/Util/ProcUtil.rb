module TOGoS ; module Util
  module ProcUtil
    def log_cmd( cmd )
      STDERR.puts "$ #{cmd}"
    end
    
    def sys( *args )
      cmd = args.collect{|a| a.inspect}.join(' ')
      log_cmd( cmd )
      system(*args) or raise "Command failed!" 
    end
    
    def php( *args )
      sys ENV['PHP'],*args
    end
  end
end ; end
