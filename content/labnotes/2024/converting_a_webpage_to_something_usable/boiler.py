from boilerpy3 import extractors

extractor = extractors.ArticleExtractor()

# From a URL
content = extractor.get_content_from_url('https://willschenk.com/fragments/2024/i_need_a_trigger_warning/')

print( content )
