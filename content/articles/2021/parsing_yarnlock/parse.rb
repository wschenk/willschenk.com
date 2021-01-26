require 'yaml'

def parse_yaml_lock data
  packages = []

  package = nil
  deps = nil
  data.each_line do |line|
    line.chomp!

    if line[0] == '#'
#      puts 'Comment'
    elsif line[0] == ' '
      parsed = line.split( " " )
#      puts "#{parsed[0]}:#{parsed[1]}"
      strip_quotes = parsed[1].gsub( /\"/, "" ) if parsed[1]
      if deps
        deps << [parsed[0], strip_quotes]
      else
        case parsed[0]
        when "version"
          package[:version] = strip_quotes
        when "resolved"
          package[:resolved] = strip_quotes
        when "integrity"
          package[:integrity] = strip_quotes
        when "dependencies:"
          deps = []
          package[:dependencies] = deps
        end
      end
    elsif line[0] == nil
#      puts 'newline'
    else
      packages << package if package
      package = {}
      deps = nil
      package_name = line.gsub( /@.*/, "" ).gsub( /"/, '' )
      package[:name] = package_name
    end
  end

  packages << package if package

  packages
end

data = File.open( '/home/wschenk/gratitude/yarn.lock' )

parsed = parse_yaml_lock data

pp parsed
