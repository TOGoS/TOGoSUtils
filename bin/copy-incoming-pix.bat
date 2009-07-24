@echo off
ruby -x %~f0 %*
if errorlevel 1 pause
goto:eof

#!ruby

require 'fileutils'

USAGE = <<EOS
Usage: copy-incoming-pix [options] [input dir] ...
Options:
  -image-dest <dir> ; dir to put images
  -video-dest <dir> ; dir to put videos
  -newer-than "YYYY-mm-dd [HH:MM:SS]" ; only process files with mtime > given
  -newer-than-file <file> ; only process files with mtime > that of given one
EOS

$input_dirs = []
$image_dest = nil
$video_dest = nil
$files_copied_count = 0
$newer_than = nil

args = $*.clone
while arg = args.shift
  case arg
  when '-image-dest'
    $image_dest = args.shift
  when '-video-dest'
    $video_dest = args.shift
  when '-newer-than'
    $newer_than = Time.parse(args.shift)
  when '-newer-than-file'
    $newer_than = File.mtime(args.shift)
  when '-h', '-?', '--help'
    STDOUT << USAGE
    exit 0
  when /^[^-]/
    $input_dirs << arg
  else
    STDERR.puts "Unrecognised argument: " + arg
    STDERR << USAGE
    exit 1
  end
end

if $input_dirs.length == 0
  $input_dirs << File.dirname($0)
end

if $image_dest == nil
  STDERR.puts "No image dest dir specified"
end

if $video_dest == nil
  STDERR.puts "No video dest dir specified"
end

def walk( dir, &prok )
  if File.directory? dir
    Dir.foreach( dir ) do |fn|
      next if fn[0] == ?.
      walk( "#{dir}/#{fn}", &prok )
    end
  else
    prok.call( dir )
  end
end

def sys( stuff )
  system stuff
end

def copy( infile, outfile )
  FileUtils.mkdir_p( File.dirname(outfile) )
  FileUtils.cp( infile, outfile )
end

def copy_mtime( infile, outfile )
  sys "touch -r \"#{infile}\" \"#{outfile}\""
end

def process_image( infile )
  return unless $image_dest
  mtime = File.mtime(infile)
  return if $newer_than and mtime <= $newer_than
  outfile = $image_dest + '/' + mtime.strftime('%Y/%m/%Y_%m_%d/%H%M%S-') + File.basename(infile)
  return if File.exist? outfile
  copy infile, outfile
  sys "jhead -autorot \"#{outfile}\""
  copy_mtime infile, outfile
  $files_copied_count += 1
end

def process_video( infile )
  return unless $video_dest
  mtime = File.mtime(infile)
  return if $newer_than and mtime <= $newer_than
  outfile = $video_dest + '/' + mtime.strftime('%Y/%m/%Y_%m_%d/%H%M%S-') + File.basename(infile)
  return if File.exist? outfile
  copy infile, outfile
  copy_mtime infile, outfile
  $files_copied_count += 1
end

for indir in $input_dirs
  walk( indir ) do |file|
    case file
    when /\.jpe?g$/i
      process_image( file )
    when /\.(?:avi|mov|wmv)$/i
      process_video( file )
    end  
  end
end

puts "#{$files_copied_count} files copied"