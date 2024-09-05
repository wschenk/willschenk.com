---
title: Styling tables with Hugo
subtitle: Markdown sometimes isn't enough
tags:
  - bootstrap
  - hugo
date: "2020-03-02"
aliases: "/articles/2020/styling_tables_with_hugo"
---

Markdown is a nice format to write it, but sometimes you need to add HTML classes directly to the output to make it look how you want. Here's a way to do that using hugo shortcodes.

## Create a `table` shortcode

Put this in `/layouts/shortcodes/table.html`:

```html
{{ $htmlTable := .Inner | markdownify }} {{ $class := .Get 0 }} {{ $old := "
<table>
  " }} {{ $new := printf "<table class=\"%s\">" $class }} {{ $htmlTable :=
  replace $htmlTable $old $new }} {{ $htmlTable | safeHTML }}
</table>
```

## Create a table

Like this:

```markdown
| Header 1 | Header 2 | Header 3 |
| -------- | -------- | -------- |
| Item 1   | Item 2   | Item 3   |
| Item 1a  | Item 2a  | Item 3a  |
```

Which should render like so:

| Header 1 | Header 2 | Header 3 |
| -------- | -------- | -------- |
| Item 1   | Item 2   | Item 3   |
| Item 1a  | Item 2a  | Item 3a  |

## Wrap it in shortcodes

<code><pre>
&#123;&#123;&#60;table "table table-striped table-bordered">&#125;&#125;
|----------|----------|----------|
| Item 1 | Item 2 | Item 3 |
| Item 1a | Item 2a | Item 3a |
&#123;&#123;&#60;/table>&#125;&#125;

</pre></code>

Which should render:

{{<table "table table-striped table-bordered">}}
| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Item 1 | Item 2 | Item 3 |
| Item 1a | Item 2a | Item 3a |
{{</table>}}

## References

1. https://zwbetz.com/style-a-markdown-table-with-bootstrap-classes-in-hugo/
