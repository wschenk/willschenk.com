def inline(file)
  data = File.read(file)

  data.gsub!( /<inline remote-html=\"(.*)\">.*?<\/inline>/ ) do |m|
    file_name = $1
    file = file_name.gsub( /^\//, "" )
    if File.exist? file
      #"<inline remote-html=\"#{file_name}\">#{File.read(file)}<\/inline>"
      File.read file
    else
      m
    end
  end

  puts data
end

inline('dynamic.html')
