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
        str = str.clone
        str.gsub!(/\\/,'\\\\')
        str.gsub!(/"/,'\\"')
        str = "\"#{str}\""
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
	  system( cmd )
	end
      end
      
      class MP3
	include Encoding

	def initialize( bitrate )
	  @bitrate = bitrate
	end
	def postfix
	  ".#{@bitrate}.mp3"
	end
	def description
	  return "#{@bitrate}kbps constant-bitrate MP3"
	end
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
	  
	  cmd = "lame -b #{@bitrate} -h #{esc infile} #{esc outfile} "
          if title  = tags['title']   ; cmd << "--tt #{esc title} "  ; end
	  if author = tags['author']  ; cmd << "--ta #{esc author} " ; end
          if album  = tags['album']   ; cmd << "--tl #{esc album} "  ; end
          if track  = tags['track']   ; cmd << "--tn #{esc track} "  ; end
          if year                     ; cmd << "--ty #{esc year} "   ; end
	  if genre  = tags['genre']   ; cmd << "--tg #{esc genre} "  ; end
	  if comm   = tags['comment'] ; cmd << "--tc #{esc comm} "   ; end
	  if caf = tags['cover-art-file'] ; cmd << "--ti #{esc caf}" ; end
	  
	  system( cmd )
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
          
	  system( cmd )
	end
	def description
	  return "Ogg encoded at quality setting #{@quality} (0=worst,10=best)"
	end
      end

      DESCRIPTIONS = <<-EOS
  mp3-<bitrate>  ; (e.g. mp3-192) constant-bitrate MP3
  ogg-q<quality> ; (e.g. ogg-q4)  Ogg with quality setting 0-10
  flac           ; FLAC
      EOS


      ByName = {
      }
      class << ByName
	def []( index )
	  if( self.include?( index ) )
	    super
	  elsif index =~ /^mp3-(\d+)$/
	    self[index] = Encodings::MP3.new( $1.to_i )
	  elsif index =~ /^ogg-q(\d+)$/
	    self[index] = Encodings::Ogg.new( $1.to_i )
	  elsif index == 'flac'
	    self[index] = Encodings::FLAC.new
	  else
	    NIL
	  end
	end
      end
    end
  end
end
