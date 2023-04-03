---
title: Why Engineers build crappy products
subtitle: looks like it was designed by an engineer
tags:
  - product
date: 2015-02-21
aliases:
  - "/why-engineers-build-crappy-products/"
---

There's a certain user interface that, when you first see it, screams out of you that it was designed by an engineer.

<img src="mainfull.png" class="img-fluid">

(_via [Daring Fireball](http://daringfireball.net) [User Interface of the Week](https://www.google.com/search?client=safari&rls=en&q=daring+fireball+user+interface+of+the+week&ie=UTF-8&oe=UTF-8)_)

Why does that happen? What is it in the nature of engineering or software development process that leads to user interfaces that are impossible for the users to actually use?

## Perfect Storm of User Discontent

The short version:

- The most interesting part of engineer's jobs is the obsession with the possibilities in the solution space - all the cool shit you can make computers do.
- Engineers don't want to make choices that limit power and flexibly.
- Engineers value adding potential functionality higher than removing unneeded complexity.
- Engineers want to engineer their way out of design problems.


## Being a people person

<p><iframe width="560" height="315" src="https://www.youtube.com/embed/RAY27NU1Jog" frameborder="0" allowfullscreen></iframe></p>

The process of thinking about a new product begins with an attempt to understand what it is that the software supposed to do, that is, it starts with specifying a solution to a problem. This is assuming that the problem itself is correctly framed, which it often isn't.  These specifications generally take form in things like user stories and wireframes which are ways of specifying both what the user is trying to do and the specific interface that the user will use to achieve those goals.

Implicit in this is the understanding of what technology is able to do. Sometimes, especially for a product with a novel technical solution, what technology is able to do it isn't completely understood. And regardless of whether or not the solution is in a known problem space, there will always be different challenges that will come up when building something.  Building software is actually very difficult.

Given that fact, engineering will look at design, start thinking about what the general shape of the solution looks like, and should structure their development efforts around the parts of the product that seem the most risky -- the parts that will be technically the most challenging. If we can prove that those parts will work, with a prototype or make it functional with a sort of mock interface, then and only then does it make sense to flush out the rest of the product.

## The fractal nature of engineering

This is the fun part. Every engineer loves greenfield development because it allows them to explore the terrain without having to deal with the trade-offs, constraints, and messiness of existing code. The process is one of exploration, of being able to explore a technical space of possibilities and potentialities. This is really awesome, and a large part of what makes engineering interesting.

The experience is of building little things, putting them together in novel ways, and build things which are increasingly powerful.  This leads engineers to put knobs and buttons and gizmos on different functionality to make it easier to explore all of what it can do.  It's socially rewarding to show off all this cool stuff, not just what it does not, but also what it's capable of doing.  Focusing on the technology aspect of it leads people to optimize the functionality in both breath and power.

## Interface ecstasy

<img src="nosql_query.png" class="img-fluid">
_Engineers Keeping it Real: Go Fuck Yourself_

The problem is products aren't about technology. Technology is necessary but not sufficient to build products. Technology is just the plumbing, the thing which makes it possible, but not actually the thing that people care about.

The ecstasy of possibility is in conflict with producing elegant products, because products are fundamentally about making choices and creating clarity around something which is very complicated. This is more than missing the forest for the trees; the ideals and aspirations of both efforts are directly contradictory.

## Example: Me doing Yet Another Dumb Thing

We were recently [building a product](http://shoutouts.happyfuncorp.com/) that would use email as an interface touch point.  Specifications, being what they are, didn't really explain what should happen when a user attempted to interact with another user who had only partially setup their account.  We were just playing around with some stuff, so it's not even fair to blame the specifications on being vague; as it often goes when building something, we ended up way deeper in the weeds than anyone intended.

Looking at this from the edge case perspective, that is, while focusing on the engineering implementation and building in robustness of response based upon stuff that could happen, my instinct was to start building out code that would deal with this.  It also uses slack as an endpoint, and in that set of scenarios it does something clever, so naturally email should do something clever as well.

I started down the path of being able to store partial requests that would get queued up and in the user interface to resolve in the future.  I started going down the path of creating a whole bunch of functionality because I was thinking about things in terms of the technology, the other interfaces had something "elegant", and I wanted to build something complete.  Madness was creeping into the interface, because I was in the mind set of searching for a technology solution.

This is not something that's core to the product.  No one actually cares. In the situation like this, the answer really isn't to be a lot of technology solution. I thought that there isn't a problem, but that a technology solution is not the most effective way to solve the problem.  It's actually a design problem.

## Technology is the easy part

If the teams aren't integrated, if the design team "finishes" their work and then lobs it over the fence to the engineering team, this conversation will take the form of engineering requesting designs for something that "was missed and we need to solve".  It may have been missed, but it doesn't need to be solved.  The solution, in this case, is not to "solve it" at all, but to reframe the problem.  And this reframing puts it into a place that engineers don't like to be.  It was not to build out a whole partially-completed-request queuing system that the user could resolve, be alerted of outstanding tasks or whatever, it was instead to simply keep the email interface simple and redirect them to the main flow on the website.

In short, the reason why engineers tend to be crappy product designers is that

- The most interesting part of engineer's jobs is the obsession with the possibilities in the solution space - all the cool shit you can make computers do.
- Engineers don't want to make choices that limit power and flexibly.
- Engineers value adding potential functionality higher than removing unneeded complexity.
- Engineers want to engineer their way out of design problems.
