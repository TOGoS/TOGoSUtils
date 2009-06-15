@echo off
ruby -x %~f0 %*
if errorlevel 1 pause
goto:eof

#!ruby

require 'fileutils'

USAGE = <<EOS
Usage: copy-incoming-pix [options] -output-dir <dir> [input dir] ...
Options:
  -newer-than "YYYY-mm-dd [HH:MM:SS]" ; only process files with mtime > given
  -newer-than-file <file> ; only process files with mtime > that of given one
EOS

$input_dirs = []
$output_dir = nil
$files_copied_count = 0
$newer_than = nil

args = $*.clone
while arg = args.shift
  case arg
  when '-output-dir'
    $output_dir = args.shift
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

if $output_dir == nil
  STDERR.puts "No output dir specified"
  STDERR << USAGE
  exit 1
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

def process( infile )
  mtime = File.mtime(infile)
  return if $newer_than and mtime <= $newer_than
  outfile = $output_dir + '/' + mtime.strftime('%Y/%m/%Y_%m_%d/%H%M%S-') + File.basename(infile)
  return if File.exist? outfile
  FileUtils.mkdir_p( File.dirname(outfile) )
  FileUtils.cp( infile, outfile )
  system "jhead -autorot \"#{outfile}\""
  $files_copied_count += 1
end

for indir in $input_dirs
  walk( indir ) do |file|
    if file =~ /\.jpe?g$/i
      process( file )
    end  
  end
end

puts "#{$files_copied_count} files copied"