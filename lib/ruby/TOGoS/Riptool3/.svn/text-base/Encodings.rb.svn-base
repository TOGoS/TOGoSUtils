module TOGoS
  module Riptool3

    # Interface for encodings
    module Encoding
      # Should return a postfix to add to filenames encoded with this format
      def postfix ; end

      # Should encode infile to outfile, tagging with tags
      # Comon tags are:
      #  author, comment, date, genre, title, year
      def encode( infile, outfile, tags ) ; end

      # Should return a description of the format
      def description ; end
    end

    module Encodings

      class MP3
	include Encoding

	def initialize( bitrate )
	  @bitrate = bitrate
	end
	def postfix
	  ".#{@bitrate}.mp3"
	end
	def description
	  return "#{@bitrate}kbps onstant-bitrate MP3"
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
	  system("lame -b #{@bitrate} -h " +
		 "#{esc infile} " +
		 "#{esc outfile} " +
		 ((author = tags['author']) ? "--ta #{esc author} " : '') +
		 ((genre = tags['genre']  ) ? "--tg #{esc genre} "  : '') +
		 ((comm = tags['comment'] ) ? "--tc #{esc comm} "   : '') +
		 ((year                   ) ? "--ty #{esc year} "   : '') +
		 ((title = tags['title']  ) ? "--tt #{esc title} "  : '') )
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
	  system("oggenc #{esc infile} " +
		 "-o #{esc outfile} -q #{@quality} " +
		 ((author = tags['author']) ? "--artist #{esc author} " : '') +
		 ((genre = tags['genre']  ) ? "--genre #{esc genre} "   : '') +
		 ((date = tags['date']    ) ? "--date #{esc date} "     : '') +
		 ((comm = tags['comment'] ) ?
		  "--comment COMMENT=#{esc comm} " : '') +
		 ((title = tags['title']  ) ? "--title #{esc title} "   : '') )
	end
	def description
	  return "Ogg encoded at quality setting #{@quality} (0=worst,10=best)"
	end
      end

      DESCRIPTIONS = <<-EOS
  mp3-<bitrate>  ; (e.g. mp3-192) constant-bitrate MP3
  ogg-q<quality> ; (e.g. ogg-q4)  Ogg with quality setting 0-10
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
	  else
	    NIL
	  end
	end
      end
    end
  end
end
