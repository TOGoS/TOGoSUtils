#!/usr/bin/ruby

require 'stringio'
require 'socket'

module TOGoS
  module Sprawxy
    class RRIO
      # Capitalize header key
      def self.chk( k )
        return k.split('-').collect{|p|p.capitalize}.join('-')
      end

      def self.read_request( cs )
        sl = cs.gets.strip
        req = Request.new
        if sl =~ /^(\S+) (\S+) (\S+)$/
          STDERR.puts sl
          req.verb = $1
          req.uri = $2
          req.protocol = $3
          while line = cs.gets and line =~ /:\s*/
            req.headers[$`.downcase] = $'.strip
          end
          if cl = req.headers['content-length']
            req.content = cs.read(cl.to_i)
          else
            req.content = ""
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
        cs.write res.content
        cs.close
      end

      def self.write_request( cs, req )
        cs.write "#{req.verb} #{req.uri} #{req.protocol}\r\n"
        for (k,v) in req.headers
          cs.write "#{chk(k)}: #{v}\r\n"
        end
        cs.write "\r\n"
        cs.write req.content
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
            res.content = cs.read(cl.to_i)
          else
            res.content = cs.read
          end
          return res
        else
          raise "Unrecognised response line: #{sl}"
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

      def host_map
        @host_map ||= {}
        @host_file_mtime ||= Time.at(0)
        @host_file = 'hosts'
        if File.exist? @host_file and File.mtime(@host_file) > @host_file_mtime
          @host_map = {}
          open(@host_file) do |s|
            while line = s.gets
              line.strip
              next if line =~ /^#/ || line == ''
              parts = line.split /\s+/
              ipaddy = parts[0]
              for name in parts[1..-1]
                @host_map[name] = ipaddy
              end
            end
          end
          @host_file_mtime = File.mtime(@host_file)
        end
        return @host_map
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
          return [$`,port]
        end
      end
    end

    class Client
      def self.instance ; @instance ||= Client.new ; end

      def do_request( req )
        if req.uri =~ %r<^http://([^/]+)/>
          hostname = $1
          path = "/#{$'}"
        else
          raise "Don't know how to handle #{req.uri}"
        end
        (host,port) = Resolver.instance.resolve(hostname)
        cs = TCPSocket.new( host, port )
        subreq = req.clone
        subreq.protocol = 'HTTP/1.0'
        subreq.uri = path
        subreq.headers['host'] = hostname
        RRIO.write_request(cs, subreq)
        res = RRIO.read_response(cs)
        return res
      end
    end

    class Server
      def handle_connection(cs)
        begin
          STDERR.puts "Connection"
          begin
            req = RRIO.read_request(cs)
            res = Client.instance.do_request( req )
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
          
          if req
            logname = Time.new.strftime('%Y%m%d%H%M%S') + '-' + req.uri.gsub(/[^a-zA-Z0-9\_\.]/,'-')
            open( 'logs/' + logname + '.log', 'w' ) do |logstream|
              RRIO.write_request( logstream, req )
              logstream.puts "-"*75
              RRIO.write_response( logstream, res )
            end
          end
          
          RRIO.write_response( cs, res )
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
