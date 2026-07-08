---
title: A format for sharing transcripts
subtitle: the prompts are the interesting part
tags:
  - ai
  - claude
  - meta
date: 2026-07-08
---

Most shared chat transcripts get it backwards. They dump the model's
output at full length — which nobody reads — and bury the actual
craft, which is what you asked for and how you steered when it went
sideways. This is a demo of a format that inverts that: prompts
prominent, my commentary in between, responses folded away unless you
want them.

{{< prompt >}}
I want to build a little analog clock as a single self-contained HTML
file. Ink and paper aesthetic: cream background, near-black lines, a
red second hand. SVG, no dependencies, keep it under 60 lines.
{{< /prompt >}}

Notice the constraints in the prompt: self-contained, a palette, a
size budget. When I leave those out, I get a React app with a build
step. The response is here if you care, but honestly the prompt tells
you more:

{{< response summary="Claude builds the clock, walks through the SVG structure" >}}
Here's a self-contained analog clock. The face is an SVG circle with
twelve tick marks generated in a loop, and the three hands are lines
rotated around the center with `transform="rotate(...)"`.

The JavaScript is one `tick()` function that reads the current time
and sets each hand's rotation — seconds at 6° per second, minutes at
6° per minute plus a little drift from the seconds, hours at 30° per
hour plus drift from the minutes. It runs once at load and then every
second with `setInterval`.

Styling stays in a small `<style>` block: `#F5F0E8` paper, `#1A1A1A`
ink for the rim and hands, `#C41E1E` for the second hand, all matching
the palette you described.
{{< /response >}}

First pass had the hour hand jumping on the hour instead of creeping.
One follow-up:

{{< prompt label="Follow-up" >}}
The hour hand should drift continuously — at 6:30 it should be halfway
between 6 and 7, not pointing at 6. Same for the minute hand between
seconds. Also add a digital readout under the clock, small and muted.
{{< /prompt >}}

{{< response summary="Claude adds fractional rotation and the readout" >}}
Good catch — I was truncating to whole units. The fix is to add the
fractional component to each hand's angle: the minute hand gets
`seconds / 10` extra degrees and the hour hand gets `minutes / 2`.

For the readout I added a `div` under the SVG with
`toLocaleTimeString()`, styled in the faded ink color with some
letter-spacing so it reads as a caption rather than the main event.
{{< /response >}}

That's the whole session — two prompts. And this is the part I really
wanted: the artifact itself, embedded live instead of as a screenshot:

{{< artifact file="clock.html" title="clock.html — ink & paper clock" height="420px" >}}

The format needs three shortcodes: `prompt` for what I typed,
`response` for what came back (collapsed, with a word count so you
know what you're signing up for), and `artifact` to drop any HTML the
session produced into a sandboxed iframe. Commentary is just regular
prose between them.
