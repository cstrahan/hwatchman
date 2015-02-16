# quick and dirty script to capture watchman file change info in BSER format,
# to be used for testing performance and correctness of the deserializer.

require 'socket'
require 'pathname'
require 'fileutils'
require 'open3'
require 'json'
require 'shellwords'

ROOT      = Pathname.new("/watchman-recording")
DIR       = Pathname.new(File.expand_path("../", __FILE__))
SOCKNAME  = DIR.join(".watchman")
MAX_ITERS = 1000

FileUtils.mkdir_p(ROOT)

def _bser_encode(obj, buffer="")
  if obj.is_a?(Array)
    buffer << "\x00"
    _bser_encode(obj.length, buffer)
    obj.each do |o|
      _bser_encode(o, buffer)
    end
  elsif obj.is_a?(String)
    buffer << "\x02"
    _bser_encode(obj.bytesize, buffer)
    buffer << obj
  elsif obj.is_a?(Fixnum)
    buffer << "\x06"
    buffer << [obj].pack("Q")
  elsif obj.is_a?(Pathname)
    _bser_encode(obj.to_s, buffer)
  else
    raise "#{obj.class.name} is not supported!"
  end
  buffer
end

def bser_encode(obj, buffer="")
  encoded = _bser_encode(obj, "")
  buffer << "\x00\x01"
  _bser_encode(encoded.bytesize, buffer)
  buffer << encoded
  buffer
end

def watchman(cmd, opts={})
  puts ">> watchman --sockname=#{SOCKNAME} #{cmd}"
  pre = ""
  if opts[:stdin]
    pre = "echo #{opts[:stdin].shellescape} |"
  end
  `#{pre} watchman --sockname=#{SOCKNAME} #{cmd}`
end

def clock
  JSON.parse(watchman "clock #{ROOT}")["clock"]
end

# capture changes in batches
data = ""
[1, 10, 100, 1000].each do |batch_size|
  watchman "shutdown-server"
  Dir.glob(ROOT.join("*")).each do |file|
    FileUtils.rm(file)
  end
  watchman "watch #{ROOT}"
  sleep 0.25
  time = clock
  1.upto(batch_size) do |x|
    puts "== #{x}/#{batch_size} =="
    FileUtils.touch(ROOT.join(x.to_s))
  end
  sleep 0.05
  UNIXSocket.open(SOCKNAME.to_s) do |socket|
    socket.write(bser_encode( ["query", ROOT, ["since", time]] ))
    socket.close_write
    data = socket.read
  end
  File.open(DIR.join("batch#{batch_size}.bin"), "wb") do |f|
    f.write(data)
  end
end

# Ditto for JSON
data = ""
[1, 10, 100, 1000].each do |batch_size|
  watchman "shutdown-server"
  Dir.glob(ROOT.join("*")).each do |file|
    FileUtils.rm(file)
  end
  watchman "watch #{ROOT}"
  sleep 0.25
  time = clock
  1.upto(batch_size) do |x|
    puts "== #{x}/#{batch_size} =="
    FileUtils.touch(ROOT.join(x.to_s))
  end
  sleep 0.05
  data = watchman "-j", :stdin => ["query", ROOT, ["since", time]].to_json
  File.open(DIR.join("batch#{batch_size}.json"), "wb") do |f|
    f.write(data)
  end
end

watchman "shutdown-server"
