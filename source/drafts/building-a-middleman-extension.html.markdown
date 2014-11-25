---
title: 'Building a Middleman Extension'
# date: TBD When publishing
tags: middleman, howto, ruby
---

## Building an extension

1. Create a gem
2. `spec.add_runtime_dependency     'middleman-core', ['>= 3.0.0']`
3. Rename `lib/middleman` to `lib/middleman-autometatags` _replace with your gem name_
4. Add `lib/middleman_extension.rb` and register your extension

```rb
require 'middleman-core'

::Middleman::Extensions.register(:autometatags) do
  require 'middleman-autometatags/extension'
  ::Middleman::Autometatags
end
```

4. Create `lib/middleman-autometatags/extension.rb` and bring in your helper methods:

```rb
require 'middleman-autometatags/helpers'

module Middleman
  module Autometatags
    class << self
      def registered(app, options_hash = {}, &block)
        app.helpers Middleman::Autometatags::Helpers
      end
    end
  end
end
```
6. 
5. 
