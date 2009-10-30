#!/usr/bin/ruby

require 'stringio'
require 'socket'
require 'fileutils'

module TOGoS
  module Sprawxy
    class ConfigFile
      def initialize( filename, &parser )
        @filename = filename
        @parser = parser
      end

      def parse( stream )
        @parser.call( stream )
      end

      def data
        filemt = File.mtime( @filename )
        if @data == nil or @mtime == nil or @mtime < filemt
          open(@filename) do |stream|
            @data = parse( stream )
          end
          @mtime = filemt
        end
        return @data
      end
    end

    class ContentStream
      def initialize( stream, length=nil )
        @stream = stream
        @length = length
      end

      def read( amount=nil )
        if l = @length
          if (amount and amount > l) or !amount
            amount = l
          end
        end
        if amount
          data = []
          while amount > 0 and f = @stream.read(amount)
            data << f
            amount -= f.length
          end
          return data.join
        else
          return @stream.read
        end        
      end

      def to_s
        return @data ||= read()
      end
    end

    class RRIO
      # Capitalize header key
      def self.chk( k )
        return k.split('-').collect{|p|p.capitalize}.join('-')
      end

      def self.read_request( cs )
        sl = cs.gets.strip
        req = Request.new
        if sl =~ /^(\S+) (\S+) (\S+)$/
          req.verb = $1
          req.uri = $2
          req.protocol = $3
          while line = cs.gets and line =~ /:\s*/
            req.headers[$`.downcase] = $'.strip
          end
          if cl = req.headers['content-length']
            req.content = ContentStream.new(  cs, cl.to_i )
          end
          
          return req
        else
          raise "Unrecognised request line: #{sl}"
        end
      end

      def self.write_response( cs, res )
        cs.write "#{res.protocol} #{res.status_code} #{res.status_text}\r\n"
        for (k,v) in res.headers
          cs.write "#{chk(k)}: #{v}\r\n"
        end
        cs.write "\r\n"
        cs.write res.content.to_s
      end

      def self.write_request( cs, req )
        cs.write "#{req.verb} #{req.uri} #{req.protocol}\r\n"
        for (k,v) in req.headers
          cs.write "#{chk(k)}: #{v}\r\n"
        end
        cs.write "\r\n"
        cs.write req.content.to_s
        cs.flush
      end

      def self.read_response( cs )
        sl = cs.gets.strip
        if sl =~ /^(\S+) (\d+) (.*)$/
          res = Response.new
          res.protocol = $1
          res.status_code = $2.to_i
          res.status_text = $3
          while line = cs.gets and line =~ /:\s*/
            res.headers[$`.downcase] = $'.strip
          end
          if cl = res.headers['content-length']
            cl = cl.to_i
          else
            cl = nil
          end
          res.content = ContentStream.new( cs, cl )
          return res
        else
          raise "Unrecognised response line: #{sl}"
        end
      end

      def self.connect_streams( *socks )
        catch(:eof) do
          while select_result = select( socks, nil, socks )
            for fromsock in select_result[0]
              begin
                data = fromsock.readpartial(1024)
                for tosock in socks
                  next if tosock == fromsock
                  tosock.write(data)
                end
              rescue IOError
                throw :eof
              end
            end
            if select_result[2].length > 0
              throw :eof
            end
          end
        end
        for sock in socks
          begin
            sock.close
          rescue IOError
          end
        end
      end
    end

    class RR
      attr_accessor :headers
      attr_accessor :content
      attr_accessor :protocol

      def initialize
        @headers = {}
        @protocol = 'HTTP/1.0'
      end

      def clone
        o = super
        o.headers = @headers.clone
        return o
      end
    end

    class Request < RR
      attr_accessor :verb
      attr_accessor :uri
    end

    class Response < RR
      attr_accessor :status_code
      attr_accessor :status_text
    end

    class ConnectResponse < Response
      attr_accessor :connect_stream
    end

    class Util
      def self.copystream( sin, sout )
        return if sin == nil
        begin
          if sin.respond_to? :readpartial
            while data = sin.readpartial(1024)
              sout.write(data)
            end
          else
            while data = sin.gets
              sout.write(data)
            end
          end
        rescue EOFError
        end
      end

      def self.copystreams( a, b )
        t1 = Thread.new { copystream(a,b) }
        copystream(b,a)
        t1.join
      end
    end

    class Resolver
      def self.instance ; @instance ||= Resolver.new ; end

      def initialize
        @host_file = ConfigFile.new( 'hosts' ) do |s|
          host_map = {}
          while line = s.gets
            line.strip!
            next if line =~ /^#/ || line == ''
            parts = line.split /\s+/
            ipaddy = parts[0]
            for name in parts[1..-1]
              host_map[name] = ipaddy
            end
          end
          host_map
        end
      end

      def host_map
        return @host_file.data
      end

      # Turn a host[:port] string into a [resolved_host,resolved_port]
      def resolve(host)
        if host =~ /:/
          host = $`
          port = $'
        else
          port = 80
        end
        if resolved = host_map[host]
          return [resolved,port]
        else
          return [host,port]
        end
      end
    end

    class Client
      def self.instance ; @instance ||= Client.new ; end

      def do_request( req )
        if req.verb == 'CONNECT'
          hostname = req.uri
          (host,port) = Resolver.instance.resolve(hostname)
          cs = TCPSocket.new( host, port )
          res = ConnectResponse.new
          res.content = nil
          res.connect_stream = cs
          res.status_code = 200
          res.status_text = 'Connection Established'
          return res
        elsif req.uri =~ %r<^http://([^/]+)/>
          hostname = $1
          path = "/#{$'}"
          (host,port) = Resolver.instance.resolve(hostname)
          if host == nil or host == ''
            raise "No host returned when resolving '#{hostname}'"
          end
          cs = TCPSocket.new( host, port )
          subreq = req.clone
          subreq.protocol = 'HTTP/1.0'
          subreq.uri = path
          subreq.headers['host'] = hostname
          RRIO.write_request(cs, subreq)
          res = RRIO.read_response(cs)
          return res
        else
          raise "Don't know how to handle #{req.uri}"
        end
      end
    end

    class Server
      def initialize
        @alias_file = ConfigFile.new( 'aliases' ) do |s|
          aliases = {}
          while line = s.gets
            line.strip!
            next if line =~ /^#/ || line == ''
            if line =~ /\s+/
              aliases[$`] = $'
            end
          end
          aliases
        end

        @loguri_file = ConfigFile.new( 'loguris' ) do |s|
          loguris = []
          while line = s.gets
            line.strip!
            next if line =~ /^#/ || line == ''
            loguris << line
          end
          loguris
        end
      end

      def should_log?( req )
        for l in @loguri_file.data
          return true if req.uri.include?( l )
        end
        return false
      end

      def aliases
        return @alias_file.data
      end

      def apply_aliases( uri )
        for (k,v) in aliases
          if uri[0...(k.length)] == k
            return v + uri[(k.length)..-1]
          end
        end
        return uri
      end

      def handle_connection(cs)
        begin
          # STDERR.puts "Connection"
          begin
            req = RRIO.read_request(cs)
            subreq = req.clone
            subreq.uri = apply_aliases( req.uri )
            STDERR.puts "#{req.verb} #{subreq.uri} #{req.protocol}"
            res = Client.instance.do_request( subreq )
            unless res.is_a? Response
              raise "Didn't get a response!"
            end
          rescue => e
            res = Response.new
            res.status_code = '500'
            res.status_text = "Proxy Error"
            res.headers = {'Content-Type'=>'text/plain'}
            res.content = "Proxy error: " + e.message + "\n\t" + e.backtrace.join("\n\t")
          end
          
          if subreq and should_log?( subreq )
            logname = Time.new.strftime('%Y/%m/%Y_%m_%d/%Y%m%d%H%M%S') + '-' + req.uri.gsub(/[^a-zA-Z0-9\_\.]/,'-')
            logfile = 'logs/' + logname + '.log'
            FileUtils.mkdir_p( File.dirname(logfile) )
            open( logfile, 'w' ) do |logstream|
              RRIO.write_request( logstream, subreq )
              logstream.puts "-"*75
              RRIO.write_response( logstream, res )
            end
            STDERR.puts "Logged #{logfile}"
          end

          RRIO.write_response( cs, res )
          if res.is_a? ConnectResponse
            RRIO.connect_streams( cs, res.connect_stream )
          end

          begin
            cs.close unless cs.closed?
          rescue IOError
            STDERR.puts "IOError while closing client connection."
          end
        rescue Errno::EPIPE
          STDERR.puts "Broken pipe."
        end
      end
      
      def run_server( serversock )
        while consock = serversock.accept
          Thread.new(consock) do |cs|
            handle_connection(cs)
          end
        end
      end
    end
  end
end

if $0 == __FILE__
  Thread.abort_on_exception = true
  TOGoS::Sprawxy::Server.new.run_server( TCPServer.new('0.0.0.0',3128) )
end
