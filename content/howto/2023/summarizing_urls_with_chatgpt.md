---
title: Summarizing URLs with ChatGPT
subtitle: save yourself some reading time
tags:
  - chatgpt
date: 2023-05-09
---

Here's a simple way to use ChatGPT's API.  I want to add a list of links 
that I find interesting to the blog, and I was getting the description of the post
from the `head`s, but I thought why not just download the whole text, 
pipe it through ChatGPT and see what it says?

## The Script

```ruby
#!/usr/bin/env ruby

require 'bundler/inline'
require 'net/http'

gemfile do
  source 'https://rubygems.org'
  gem 'nokogiri'
  gem 'json'
  gem 'ruby-openai'
end

if ARGV[0].nil?
  puts "summarize <url>"
  exit 1
end

uri = URI(ARGV[0])
response = Net::HTTP.get_response(uri)

puts "Loading #{uri}"
doc = Nokogiri::HTML(response.body)

doc.xpath('//script').remove # remove script tags
doc.xpath('//style').remove  # remove style tags
text = doc.xpath('//text()').text.strip.gsub(/\s\s/, "")

puts "Calling ChatGPT"

token = `lpass show "OpenAI Key" | awk '/Notes/ {print $2}'`.chomp
client = OpenAI::Client.new(access_token: token)

content = "#{text}\n\ntl;dr\n"
response = client.chat(
  parameters: {
      model: "gpt-3.5-turbo", # Required.
      messages: [{ role: "user", content: content}], # Required.
      temperature: 0.7,
  })

puts "\n"
puts response.dig("choices", 0, "message", "content")
```

_Note: You need to have an [OpenAI API key](https://platform.openai.com/account/api-keys), and this script pulls it from LastPass_

## The Results

### [Stanford Director: AI Sceintists' "Frontal Cortex is massively underdeveloped"](https://futurism.com/the-byte/stanford-ai-scientists-frontal-cortex-underdeveloped)


> Robert Reich, associate director of Stanford's Institute for Human-Centered Artificial Intelligence, likened AI researchers to "late-stage teenagers" with "massively underdeveloped" frontal cortexes, which affects their sense of social responsibility when it comes to ethics in the industry. Reich's analogy highlights the risk-taking and invincibility often associated with teenagers, which he sees reflected in the AI industry's push to build technology regardless of potential consequences. While the industry is young, it is being led by adults who are making conscious choices that will impact society.

### [TikTok spied on me. Why?](https://arstechnica.com/tech-policy/2023/05/tiktok-spied-on-me-why/)

> A Financial Times journalist shares her experience of discovering she had been surveilled by TikTok as part of an attempt to find the sources of damaging stories about the social media app's Chinese parent company, ByteDance. The journalist notes that TikTok's rapid growth has made it a target for Beijing's recent crackdown on Big Tech, and there is alarm about the security of user data, particularly in the US, TikTok's biggest market. TikTok has said it has never given data to the Chinese government and wouldn't do so if asked.

### [The seven programming ur-languages](https://madhadron.com/programming/seven_ur_languages.html)

> The author identifies seven ur-languages in programming: ALGOL, Lisp, ML, Self, Forth, APL, and Prolog. These ur-languages have distinct collections of fundamentals that form the basis of programming languages today. The author recommends learning a language from each ur-language to develop neural pathways and explore new possibilities in programming. The recommended languages are PLT Racket (Lisp), Haskell (ML), Self (Self), Prolog, gForth (Forth), and K (APL).

## A Regular script

Here's another simple one that pulls out some metadata:

```ruby
#!/usr/bin/env ruby
require 'bundler/inline'
require 'net/http'

gemfile do
  source 'https://rubygems.org'
  gem 'nokogiri'
  gem 'json'
end

if ARGV[0].nil?
  puts "Usage:"
  puts "link_info url"
  exit 1
end

def fetch_url(url)
  uri = URI(url)
  response = Net::HTTP.get_response(uri)
  return response.body, response['last-modified']
end

def extract_information(html)
  doc = Nokogiri::HTML(html)

  ret = {}
  ret[:doc_title] = doc.at_css('title').content.strip rescue 'N/A'
  ret[:author] = doc.at_css("meta[name='author']")['content'].strip rescue 'N/A'
  ret[:ogtitle] = doc.at_css("meta[property='og:title']")['content'].strip rescue 'N/A'
  ret[:ogdescription] = doc.at_css("meta[property='og:description']")['content'].strip rescue 'N/A'

  ret
end

def main url
  html, last_modified = fetch_url(url)
  info = extract_information(html)

  puts JSON.pretty_generate( info )

end

main ARGV[0]
```

## Compare the results

### [Stanford Director: AI Sceintists' "Frontal Cortex is massively underdeveloped"](https://futurism.com/the-byte/stanford-ai-scientists-frontal-cortex-underdeveloped)

```
{
  "doc_title": "Stanford Director: AI Scientists’ “Frontal Cortex Is Massively Underdeveloped”",
  "author": "N/A",
  "ogtitle": "Stanford Director: AI Scientists’ “Frontal Cortex Is Massively Underdeveloped”",
  "ogdescription": "The associate director of Stanford's Institute for Human-Centered Artificial Intelligence compared AI scientists to \"late-stage\" teens."
}
```

### [TikTok spied on me. Why?](https://arstechnica.com/tech-policy/2023/05/tiktok-spied-on-me-why/)
```
{
  "doc_title": "TikTok spied on me. Why? | Ars Technica",
  "author": "N/A",
  "ogtitle": "TikTok spied on me. Why?",
  "ogdescription": "A Financial Times journalist writes about discovering she’d been surveilled by TikTok."
}
```

### [The seven programming ur-languages](https://madhadron.com/programming/seven_ur_languages.html)

```
{
  "doc_title": "madhadron  - The seven programming ur-languages",
  "author": "N/A",
  "ogtitle": "N/A",
  "ogdescription": "N/A"
}
```

## Conclusion

It didn't take any longer to write the ChatGPT based version, and while it gave different
info it was a lot better.

