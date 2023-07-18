require 'erb'

template = ERB.new File.read( "simple.erb" ), trim_mode: "<>" 
puts template.run(binding)
