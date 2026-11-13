---
title: Why is the sky dark at night?
subtitle: reading cosmology off the ceiling
tags:
  - ai
  - physics
  - transcripts
date: 2026-11-12T20:14:32
---

This has been rattling around my head since I was a kid lying in the
yard: if the universe is that old and that full of stars, the night
sky seems like it should be a wall of light. It turns out the
darkness is the interesting part.

(Built as a worked example of [this transcript format](/fragments/2026/transcript_format/): prompts upfront, model responses folded away, and the interactive toys inline.)

{{< prompt >}}
The universe is about 14 billion years old and has something like
10²² stars. If I draw a straight line from my eye out into space in
any direction, shouldn't it eventually hit the surface of a star?
Why isn't the whole night sky as bright as the sun?
{{< /prompt >}}

The intuition in the prompt is actually right, which is what makes it
a good question — in an infinite, eternal, unchanging universe every
sight line *does* end on a star, and the sky would glow at the surface
temperature of one everywhere, forever. No night. The reason ours
doesn't is the good part:

{{< response summary="Olbers' paradox — the sky is dark because the stars haven't been on forever" >}}
This is Olbers' paradox, and your geometric intuition is correct:
in an infinite, static, eternally-old universe, every line of sight
terminates on a stellar surface, and the entire sky would blaze at
roughly 5,800 K. Doubling the distance makes each star four times
dimmer, but the shell at that distance holds four times as many
stars — the effects cancel exactly.

Dust doesn't save you: in an eternal universe, dust absorbing
starlight heats up until it re-radiates just as brightly as what it
absorbs.

The sky is dark for two reasons. First and most important, the
universe has a finite age: light has only had 13.8 billion years to
travel, so we can only see stars inside that horizon — and there
simply aren't enough of them along most sight lines. Second, the
universe is expanding, so light from the most distant sources is
redshifted out of the visible band and its energy diluted.

The darkness of the night sky is therefore direct, naked-eye evidence
that the universe is not infinitely old — that it had a beginning.
{{< /response >}}

The shell cancellation is the trap: step out to distance `kR` and each
star fades by `1/k²`, which feels like it should rescue the dark sky.
Except the wedge of space at `kR` captures `k²` times as many stars, so
every concentric shell delivers the exact same `1.00×` slice of light —
click between the shells below or stack them together to watch the dark
patch disappear:

{{< artifact file="shells.html" title="shells.html — why k² stars and 1/k² brightness cancel in every shell" height="400px" wide="true" >}}

The inversion is what got me: the darkness isn't an absence of
information, it *is* the information. You can stand in a field, look
up, and rule out an eternal static cosmos with no equipment at all.
So I pushed on where the "bright sky" went:

{{< prompt label="Follow-up" >}}
So the dark sky is evidence the universe had a beginning? What would
the sky look like if the universe really were infinite and eternal —
and is the cosmic microwave background basically that bright sky,
just redshifted?
{{< /prompt >}}

{{< response summary="The bright sky exists — it's just been stretched into microwaves" >}}
Yes on both counts. An eternal static universe gives you a sky that
is everywhere as bright as the surface of a star — about 5,800 K in
every direction, day and "night" indistinguishable.

And the CMB is exactly the kind of thing your question suggests: we
*do* see a wall of light covering the entire sky, from the moment the
universe first became transparent, 380,000 years after the Big Bang.
It started as a ~3,000 K orange-white glow filling every direction.
Expansion has stretched those wavelengths by a factor of about 1,100,
so today it arrives as 2.7 K microwave radiation — invisible to your
eyes, unmissable to a radio telescope. The sky is still blazing in
every direction; it's just been redshifted out of sight.

Historical footnote: Edgar Allan Poe, in *Eureka* (1848), proposed
that the sky is dark because light from the most distant stars hasn't
had time to reach us — essentially the modern answer, seventy years
before cosmology could back him up.
{{< /response >}}

Poe got there first, in a prose poem, in 1848:

> Were the succession of stars endless, then the background of the sky would present us an uniform luminosity, like that displayed by the Galaxy — since there could be absolutely no point, in all that background, at which would not exist a star. The only mode, therefore, in which, under such a state of affairs, we could comprehend the voids which our telescopes find in innumerable directions, would be by supposing the distance of the invisible background so immense that no ray from it has yet been able to reach us at all.

And I love that the sky actually *is* uniformly bright right now —
we're just tuned to the wrong channel. Slide forward from
recombination to today and switch between your eyes and a 160 GHz
microwave dish to see what is actually overhead:

{{< artifact file="cmb.html" title="cmb.html — tuning between human eyes and the 2.7 K microwave sky" height="430px" wide="true" >}}

At this point I wanted to *see* the horizon geometry from the ground
up, so:

{{< prompt label="Follow-up" >}}
Build me a toy version I can play with: a 2D star field with an
observer at the center, sight lines cast in every direction, and a
slider for how long light has had to travel. Sight lines that reach a
star light up. Show what fraction of the sky is lit, give me a
"today" button at 13.8 Gyr and let me run the clock out to "forever."
Single self-contained HTML file, ink and paper style.
{{< /prompt >}}

{{< response summary="Claude precomputes the ray–star intersections and hangs it on a log slider" >}}
The trick that makes it responsive: for each of the 540 sight lines I
precompute, once at load, the distance at which it first intersects a
star (standard ray–circle intersection over a deterministic field of
24,000 stars). After that, moving the slider is just a comparison —
a sight line is "lit" if its first hit is inside the current light
horizon — so the whole thing re-renders instantly.

The slider is logarithmic from 1 to 2,000 billion years, since the
interesting behavior spans orders of magnitude. The view auto-zooms
to keep the horizon in frame: at small ages you see a huge dark
horizon with a few lit lines, and as the age grows the field fills
until every direction terminates on a star. Stars beyond the horizon
render as faint ghosts — they exist, but their light hasn't reached
you yet.
{{< /response >}}

Here it is, paired with what that exact slice of universe looks like
looking up above the pines in the yard. It starts at today — press
**Run clock** (or drag toward *Forever*) to watch the dark directions
run out, hover any ray to inspect when its light arrives, or toggle
**Expanding (redshift)** to see how expansion fades distant shells back
out of view:

{{< artifact file="olbers.html" title="olbers.html — every sight line ends on a star, eventually" height="820px" wide="true" >}}

Even this cartoon universe — vastly denser with stars than the real
one — is mostly dark at 13.8 billion years. Give light ten times
longer to travel in a static cosmos and the sky above the treeline
starts to close up; run it to forever and there are no dark directions
left, which is Olbers' bright sky. Flip **Expanding** on and the
geometric intersections still happen, yet redshift keeps the sky dark.

So what do we learn from a dark ceiling? That the universe had a
beginning, that it's expanding, and that the "missing" wall of light
is still out there — redshifted into microwaves — all from the
single easiest astronomical observation there is: go outside at
night and notice that it's dark.
