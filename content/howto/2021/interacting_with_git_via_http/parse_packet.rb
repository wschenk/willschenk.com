# Adapted from page 543 of Building Git
def recv_packet input
  head = input.read(4)
  return head unless /[0-9a-f]{4}/ =~ head

  size = head.to_i(16)
  return nil if size == 0

  input.read(size - 4).sub(/\n$/, "")
end

File.open( "ref.bin", "rb" ) do |input|
  while !input.eof?
    packet = recv_packet input
    if packet
      line, options = packet.split( "\0", 2 )
      puts options if options
      puts line
    end
  end
end
