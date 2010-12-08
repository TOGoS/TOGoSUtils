#!/usr/bin/ruby

require 'stringio'
require 'socket'
require 'fileutils'
require 'uri'
require 'cgi'

module TOGoS
  module Sprawxy
    class Util
      def self.xml_escape( text )
        return CGI.escapeHTML( text )
      end

      def self.guess_content_type( path )
        if path =~ /\.([^\.]+)$/
          ext = $1.downcase
          case ext
          when 'jpg'  ; return 'image/jpeg'
          when 'png'  ; return 'image/png'
          when 'txt'  ; return 'text/plain'
          when 'log'  ; return 'text/plain'
          when 'html' ; return 'text/html'
          when 'css'  ; return 'text/css'
          when 'xml'  ; return 'text/xml'
          when 'js'   ; return 'text/javascript'
          when 'json' ; return 'application/json'
          end
        end
        return 'application/octet-stream'
      end

      def self.uri_to_path( uri )
        if uri =~ /^file:\/\/(?=\/)/
          return URI.unescape($')
        elsif uri =~ /^\//
          return uri # Already path
        else
          raise "Don't know how to convert '#{uri}' to a path"
        end
      end
      
      def self.path_to_uri( path )
        if path =~ /^file:/
          return path # Already a URI
        elsif path =~ /^\//
          return 'file://' + URI.escape(path)
        else
          raise "Don't know how to convert '#{path}' to a URI"
        end
      end

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

    class ConfigFile
      def initialize( filename, default=nil, &parser )
        @filename = filename
        @parser = parser
        @default = default
      end

      def parse( stream )
        @parser.call( stream )
      end

      def data
        return @default unless File.exist?( @filename )
        filemt = File.mtime( @filename )
        if @data == nil or @mtime == nil or @mtime < filemt
          open(@filename) do |stream|
            @data = parse( stream )
          end
          @mtime = filemt
        end
        return @data
      end

      def self.parse_matcher( token )
        case token
        when /^re:/
          # User-provided regular expression
          mstr = $'
          return Regexp.new(mstr)
        when /^ex:/
          # Exact match
          mstr = $'
          return Regexp.new('^'+Regexp.escape(mstr)+'$')
        when /^inc:/
          # Includes this string
          mstr = $'
          return Regexp.new(Regexp.escape(mstr))
        else
          # Default is 'starts with'
          mstr = token
          return Regexp.new('^'+Regexp.escape(mstr))
        end
      end
    end

    class MappingFile < ConfigFile
      def initialize( filename, &processor )
        super( filename, {} ) do |s|
          mappings = {}
          while line = s.gets
            line.strip!
            next if line =~ /^\s*(?:#.*)?$/
            if processor
              kv = processor.call(line)
            else
              kv = line.split(/\s+/)
            end
            if kv
              mappings[kv[0]] = kv[1]
            end
          end
          mappings
        end
      end
    end

    class ContentStream
      def initialize( stream, length=nil )
        @stream = stream
        @length = length
        @position = 0
      end

      protected
      def _adjust_read_amount( amount )
        # If length is known, adjust amount to
        # ensure we don't read past the end of the stream.
        if @length
          if amount
            if amount > (@length-@position)
              amount = @length - @position
            end
          else
            amount = @length-@position # To end
          end
        end
        return amount
      end

      public
      attr_reader :length

      def readpartial( maxlen, outbuf )
        amount = _adjust_read_amount(amount)
        if amount == 0
          return nil
        elsif amount
          outbuf = @stream.readpartial( amount, outbuf )
        else
          outbuf = @stream.readpartial( 4096, outbuf )
        end        
        @position += outbuf.length
        return outbuf
      end

      def read( amount=nil )
        amount = _adjust_read_amount(amount)
        if amount
          outbuf = @stream.read( amount )
        else
          outbuf = @stream.read
        end
        @position += outbuf.length
        return outbuf
      end

      def to_s
        return @data ||= read()
      end

      def read_started?
        return @position > 0
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
          while line = cs.gets
            line.rstrip!
            if line.length == 0
              break
            end
            if line =~ /:\s*/
              req.headers[$`.downcase] = $'
            else
              STDERR.puts "Received weird header line from client: #{line}"
            end
          end
          if cl = req.headers['content-length']
            req.content = cs.read( cl.to_i )
          end
          
          return req
        else
          raise "Unrecognised request line: #{sl}"
        end
      end

      def self.write_response( cs, res, savecontent=false )
        cs.write "#{res.protocol} #{res.status_code} #{res.status_text}\r\n"
        for (k,v) in res.headers
          cs.write "#{chk(k)}: #{v}\r\n"
        end
        cs.write "\r\n"

        if res.content.is_a? ContentStream
          if res.content.read_started?
            raise "Cannot output ContentStream - read already started"
          end
          alldat = ""
          begin
            data = ""
            while data = res.content.readpartial( 4096, data ) and data.length > 0
              cs.write data
              alldat << data if savecontent
            end
          rescue EOFError
          end
          res.content = alldat if savecontent
        elsif res.content != nil
          str = res.content.to_s
          cs.write str
        end
        cs.flush
      end

      def self.write_request( cs, req )
        cs.write "#{req.verb} #{req.uri} #{req.protocol}\r\n"
        for (k,v) in req.headers
          cs.write "#{chk(k)}: #{v}\r\n"
        end
        cs.write "\r\n"
        if req.content != nil
          cs.write req.content.to_s
        end
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
                  tosock.flush
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
      attr_accessor :context
      def initialize
        super
        @context = {}
      end
      def clone
        k = super
        k.context = @context.clone
        return k
      end
    end

    class Response < RR
      attr_accessor :status_code
      attr_accessor :status_text
    end

    class ConnectResponse < Response
      attr_accessor :connect_stream
    end

    class Resolver
      def self.instance ; @instance ||= Resolver.new ; end

      def initialize
        @host_file = ConfigFile.new( 'hosts', {} ) do |s|
          host_map = {}
          while line = s.gets
            line.strip!
            next if line =~ /^\s*(?:#.*)?$/
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
          ss = TCPSocket.new( host, port )
          res = ConnectResponse.new
          res.content = nil
          res.connect_stream = ss
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
          ss = TCPSocket.new( host, port )
          subreq = req.clone
          subreq.protocol = 'HTTP/1.0'
          subreq.uri = path
          subreq.headers['host'] = hostname
          RRIO.write_request(ss, subreq)
          res = RRIO.read_response(ss)
          return res
        elsif req.uri =~ %r<^file://(?=/)>
          path = URI.decode($')
          subres = Response.new
          # TODO: Return a File type that can be written lazily instead of loading to string
          # TODO: Split out directory index generation
          if File.exist?( path )
            subres.status_code = 200
            if File.directory?( path )
              if path[-1] == ?/
                prefix = ''
                dir = path[0..-1]
              else
                path =~ /\/([^\/]+)$/
                prefix = $1+'/'
                dir = path
              end
              subres.status_text = "You've got directory"
              subres.headers['content-type'] = 'text/html';
              title = "Index of #{path}"
              subres.content = "<html>\n" \
                "<head><title>#{Util.xml_escape(title)}</title></head>\n" \
                "<body>\n\n" \
                "<h2>#{Util.xml_escape(title)}</h2>\n\n" \
                "<ul>\n"
              entries = []
              Dir.foreach( path ) do |fn|
                next if fn[0] == ?.
                entries << fn
              end
              entries.sort! do |a,b|
                apath = "#{dir}/#{a}"
                bpath = "#{dir}/#{b}"
                adir = File.directory?(a)
                bdir = File.directory?(b)
                if adir && !bdir
                  -1
                elsif bdir && !adir
                  1
                else
                  a.downcase <=> b.downcase
                end
              end
              for fn in entries
                depath = "#{dir}/#{fn}"
                name = fn
                if File.directory? depath
                  name += '/'
                end
                href = URI.escape(prefix + name)
                subres.content << "<li><a href=\"#{Util.xml_escape(href)}\">#{Util.xml_escape(name)}</a></li>\n"
              end
              subres.content << "</ul>\n\n"
              if sig = req.context['server-signature']
                subres.content << "<hr />\n"
                subres.content << "<p>" + Util.xml_escape(sig) + "</p>\n\n"
              end
              subres.content << "</body>\n</html>\n"
            else
              subres.status_text = "You've got file"
              subres.headers['content-type'] = Util.guess_content_type( path )
              subres.content = File.read( path )
            end
          else
            subres.status_code = 404
            subres.status_text = 'File not found'
            subres.headers['content-type'] = 'text/plain'
            subres.content = "Could not find file: #{path}"
          end
          return subres
        else
          raise "Don't know how to handle #{req.uri}"
        end
      end
    end

    class HttpProxyClient < Client
      def initialize( proxyurl )
        if proxyurl =~ %r<^http://([^:/]+)(?::(\d+))?(/|$)>
          @proxy_host = $1
          @proxy_port = ($2 && $2.to_i) || 80
        else
          raise "Could not parse proxy URL as HTTP proxy: '#{proxyurl}'"
        end
      end

      def resolved_proxy_host
        (proxyhost,) = Resolver.instance.resolve(@proxy_host)
        return proxyhost
      end

      def do_request( req )
        if req.verb == 'CONNECT'
          hostname = req.uri
          ss = TCPSocket.new( resolved_proxy_host, @proxy_port )
          ss.write "CONNECT #{hostname} HTTP/1.1\r\n"
          ss.write "Host: #{hostname}\r\n"
          ss.write "\r\n"
          ss.flush
          subres = RRIO.read_response
          if subres.status_code < 200 or subres.status_code > 200
            return subres
            #raise "CONNECT via proxy #{@proxy_host}:#{@proxy_port} failed: #{subres.status_code} #{subres.status_text}"
          end
          res = ConnectResponse.new
          res.content = nil
          res.connect_stream = ss
          res.status_code = 200
          res.status_text = 'Connection Established'
          return res
        elsif req.uri =~ %r<^http://([^/]+)/>
          begin
            ss = TCPSocket.new( resolved_proxy_host, @proxy_port )
          rescue Exception => e
            raise e.class, "Error connecting to proxy #{resolved_proxy_host}:#{@proxy_port}: #{e}", e.backtrace
          end
          subreq = req.clone
          subreq.protocol = 'HTTP/1.0'
          RRIO.write_request(ss, subreq)
          res = RRIO.read_response(ss)
          return res
        else
          raise "HTTP Proxy cannot handle #{req.uri}"
        end
      end
    end

    class Server
      def initialize
        @alias_file = MappingFile.new( 'aliases' ) do |line|
          if line =~ /\s+/
            key = ConfigFile.parse_matcher($`)
            value = $'
            [key,value]
          else
            nil
          end
        end
        @overrides_file = MappingFile.new( 'overrides' ) do |line|
          if line =~ /\s+/
            key = ConfigFile.parse_matcher($`)
            value = Util.path_to_uri( $' )
            [key,value]
          else
            nil
          end
        end
        @proxy_file = MappingFile.new( 'proxies' ) do |line|
          if line =~ /\s+/
            if $` == 'default'
              key = //
            else
              key = ConfigFile.parse_matcher($`)
            end
            value = $'
            [key,value]
          else
            nil
          end          
        end
        @loguri_file = ConfigFile.new( 'loguris', [] ) do |s|
          loguris = []
          while line = s.gets
            line.strip!
            next if line =~ /^#/ || line == ''
            loguris << ConfigFile.parse_matcher(line)
          end
          loguris
        end
      end

      attr_writer :server_name
      attr_writer :server_id
      attr_writer :server_signature

      def server_name
        return @server_name ||= 'unnamed'
      end

      def server_id
        return @server_id ||= (self.server_name + ';inst' + rand(99999999999).to_s)
      end

      def server_signature
        return @server_signature ||= "Sprawxy server #{self.server_id}"
      end

      def should_log?( req )
        for l in @loguri_file.data
          return true if l === req.uri
        end
        return false
      end

      def aliases
        return @alias_file.data
      end

      def apply_aliases( uri )
        for (k,v) in aliases
          if md = k.match(uri)
            return v + md.post_match
          end
        end
        return uri
      end

      def override_aliases
        return @overrides_file.data
      end

      def apply_override_aliases( uri )
        for (k,v) in override_aliases
          if md = k.match(uri)
            fileuri = v + md.post_match
            filepath = Util.uri_to_path( fileuri )
            if File.exist?( filepath ) and !File.directory?( filepath )
              return Util.path_to_uri( fileuri )
            end
          end
        end
        return uri
      end

      def proxies
        @proxy_file.data
      end

      def proxy_for( uri )
        for (k,v) in proxies
          if md = k.match( uri )
            return v
          end
        end
        return nil
      end

      def handle_connection(cs)
        begin
          # STDERR.puts "Connection"
          begin
            req = RRIO.read_request(cs)
            if req.headers['x-sps-id'] == self.server_id
              raise "Hey it's too loopy in here!"
            end
            origuri = req.uri
            subreq = req.clone
            subreq.context['server-signature'] = server_signature
            if subreq.uri[0] == ?/ and host = subreq.headers['host']
              newuri = 'http://'+host+subreq.uri
              subreq.uri = newuri
            end
            subreq.uri = apply_aliases( subreq.uri )
            subreq.uri = apply_override_aliases( subreq.uri )
            subreq.headers['x-sps-id'] = self.server_id
            if proxy = proxy_for( subreq.uri )
              STDERR.puts "#{req.verb} #{origuri} -> #{subreq.uri} #{req.protocol} via #{proxy}"
              res = HttpProxyClient.new( proxy ).do_request( subreq )
            else
              STDERR.puts "#{req.verb} #{origuri} -> #{subreq.uri} #{req.protocol}"
              res = Client.instance.do_request( subreq )
            end
            unless res.is_a? Response
              raise "Didn't get a response!"
            end
          rescue => e
            res = Response.new
            res.status_code = '500'
            res.status_text = "Proxy Error"
            res.headers = {'Content-Type'=>'text/plain'}
            res.content = "Proxy error: " << e.message << "\n\t" << e.backtrace.join("\n\t") <<
              "\n\n----------------\n\n" << server_signature
          end
          
          if res.content.is_a? String or res.content.is_a? ContentStream
            if l = res.content.length
              res.headers['content-length'] = l.to_s
            end
          end

          if subreq and should_log?( subreq )
            logname = Time.new.strftime('%Y/%m/%Y_%m_%d/%Y%m%d%H%M%S') + '-' + req.uri.gsub(/[^a-zA-Z0-9\_\.]/,'-')
            logfile = 'logs/' + logname + '.log'
            FileUtils.mkdir_p( File.dirname(logfile) )
            open( logfile, 'wb' ) do |logstream|
              RRIO.write_request( logstream, subreq )
              if subreq.content.is_a? String and subreq.content[-1] != ?\n
                logstream.write "\r\n"
              end
              logstream.write "-"*75 + "\r\n"
              RRIO.write_response( logstream, res, true )
            end
            STDERR.puts "Logged #{logfile}"
          end

          RRIO.write_response( cs, res )
          if res.is_a? ConnectResponse
            RRIO.connect_streams( cs, res.connect_stream )
          end

          begin
            unless cs.closed?
              cs.flush
              cs.close
            end
          rescue IOError
            STDERR.puts "IOError while closing client connection."
          end
        rescue Errno::ECONNRESET
          STDERR.puts "Connection reset by peer."
        rescue Errno::EPIPE
          STDERR.puts "Broken pipe."
        end
      end
      
      def run_server( serversock )
        while consock = serversock.accept
          Thread.new(consock) do |cs|
            cs.binmode
            cs.sync = false
            handle_connection(cs)
          end
        end
      end
    end
  end
end

if $0 == __FILE__
  listenport = 3128
  servername = nil

  args = $*.clone
  while arg = args.shift
    case arg
    when '-port' ; listenport = args.shift.to_i
    when '-server-name' ; servername = args.shift
    else
      raise "Unrecognised arg: #{arg}"
    end
  end

  Thread.abort_on_exception = true
  server = TOGoS::Sprawxy::Server.new
  server.server_name = servername
  server.run_server( TCPServer.new('0.0.0.0',listenport) )
end
