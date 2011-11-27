module TOGoS
  module Riptool3

    # Interface for encodings
    module Encoding
      # Should return a postfix to add to filenames encoded with this format
      def postfix ; end

      # Should encode infile to outfile, tagging with tags
      # Comon tags are:
      #  title   = title of track
      #  author  = name of artist/author
      #  album   = name of album
      #  track   = track number ["/" total track count]
      #  date    = YYYY-MM-DD of release
      #  year    = year of release (alternative to specific date)
      #  genre   = name of genre
      #  comment = misc. info about track
      def encode( infile, outfile, tags ) ; end

      # Should return a description of the format
      def description ; end
      
      def esc( str )
        str = str.to_s.clone
        str.gsub!(/\\/,'\\\\')
        str.gsub!(/"/,'\\"')
        str = "\"#{str}\""
      end
      
      def sys( cmd )
        if cmd.is_a? Array
          sys cmd.collect{|a| esc(a)}.join(" ")
        else
          STDERR.puts "$ #{cmd}"
          system cmd
        end
      end
    end

    module Encodings
      class FLAC
        include Encoding
        
        def initialize(compression=8)
          @compression = compression
        end
	def postfix
	  '.flac'
	end
	def description
	  "FLAC (compression #{@compression})"
	end
	def encode( infile,outfile,tags )
	  tf = []
	  if v = tags['title']  ; tf << "--tag=TITLE=#{v}"  ; end
	  if v = tags['author'] ; tf << "--tag=ARTIST=#{v}" ; end
          if v = tags['album']  ; tf << "--tag=ALBUM=#{v}"  ; end
          if v = tags['track']  ; tf << "--tag=TRACKNUMBER=#{v}" ; end
	  if v = tags['date'] || tags['year']
	    tf << "--tag=DATE=#{v}"
	  end
	  if v = tags['genre'] ; tf << "--tag=GENRE=#{v}" ; end
	  if v = tags['comment'] ; tf << "--tag=COMMENT=#{v}" ; end
	  if v = tags['cover-art-file'] ; tf << "--picture=#{v}" ; end
	  
	  cmd = "flac -#{@compression} #{esc infile} -o #{esc outfile} " +
	    tf.collect{|t| esc(t) }.join(' ')
	  sys cmd
	end
      end
      
      class AbstractMP3
        include Encoding

        def encode( infile,outfile,tags )
          unless year = tags['year']
            if date = tags['date']
              if date =~ /^(\d\d\d\d)(-\d\d?-\d\d?)?$/
                year = $1
              elsif date =~ /^\d\d\/\d\d\/(\d\d\d\d)/
                year = $1
              end
            end
          end
          
          cmd = ['lame','-q','0'] # Highest quality, slowest preprocessing!
          cmd.concat( encoding_args )
          cmd << infile
          cmd << outfile
          if title  = tags['title']   ; cmd << "--tt" << title  ; end
          if author = tags['author']  ; cmd << "--ta" << author ; end
          if album  = tags['album']   ; cmd << "--tl" << album  ; end
          if track  = tags['track']   ; cmd << "--tn" << track  ; end
          if year                     ; cmd << "--ty" << year   ; end
          if genre  = tags['genre']   ; cmd << "--tg" << genre  ; end
          if comm   = tags['comment'] ; cmd << "--tc" << comm   ; end
          if caf = tags['cover-art-file'] ; cmd << "--ti" << caf; end
          
          sys cmd
        end
        
        def encoding_args
          raise "Can't use "+self.class+" directly; use a subclass that implements #encoding_args."
        end
      end
      
      class CBRMP3 < AbstractMP3
        def initialize( bitrate=192 )
          @bitrate = bitrate
        end
        def postfix
          ".#{@bitrate}.mp3"
        end
        def description
          return "#{@bitrate}kbps constant-bitrate MP3"
        end
        def encoding_args
          return ['-b',@bitrate.to_s]
        end
      end
      MP3 = CBRMP3
      
      class VBRMP3 < AbstractMP3
        def initialize( quality=4 )
          @quality = quality
        end
        def postfix
          ".v#{@quality}.mp3"
        end
        def description
          return "quality #{@quality} variable-bitrate MP3"
        end
        def encoding_args
          return ['-V',@quality.to_s]
        end
      end

      class Ogg
	include Encoding

	def initialize( quality )
	  @quality = quality
	end
	def postfix
	  ".q#{@quality}.ogg"
	end
	def encode( infile,outfile,tags )
          date = tags['date'] || tags['year']
          
          cmd = "oggenc #{esc infile} -o #{esc outfile} -q #{@quality} "
          
          if title  = tags['title']   ; cmd << "--title #{esc title} "     ; end
          if author = tags['author']  ; cmd << "--artist #{esc author} "   ; end
          if album  = tags['album']   ; cmd << "--album #{esc album} "     ; end
          if track  = tags['track']   ; cmd << "--tracknum #{esc track} "  ; end
          if date                     ; cmd << "--date #{esc date} "       ; end
          if genre  = tags['genre']   ; cmd << "--genre #{esc genre} "     ; end
          if comm   = tags['comment'] ; cmd << "--comment COMMENT=#{esc comm} " ; end
          
          sys cmd
	end
	def description
	  return "Ogg encoded at quality setting #{@quality} (0=worst,10=best)"
	end
      end

      DESCRIPTIONS = <<-EOS
  mp3-<bitrate>      ; (e.g. mp3-192) constant-bitrate MP3
  mp3-v<compression> ; (e.g. mp3-v3)  variable-bitrate MP3;
                     ; lower value is higher quality
  ogg-q<quality>     ; (e.g. ogg-q4)  Ogg with quality setting 0-10
  flac               ; FLAC
      EOS
      
      ByName = Hash.new do |hash,index|
        if index =~ /^mp3-v(\d+)$/
          hash[index] = Encodings::VBRMP3.new( $1.to_i )
        elsif index =~ /^mp3-(\d+)$/
          hash[index] = Encodings::CBRMP3.new( $1.to_i )
        elsif index =~ /^ogg-q(\d+)$/
          hash[index] = Encodings::Ogg.new( $1.to_i )
        elsif index == 'flac'
          hash[index] = Encodings::FLAC.new
        else
          nil
        end
      end
    end
  end
end
