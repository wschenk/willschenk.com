---
title: "React Conf 2018 Recap"
date: 2018-10-28
origin: http://frantic.im/react-conf-2018
alternate: http://frantic.im/react-conf-2018
type: link
---

<p>This was more exciting than a couple of previous WWDC, combined! The big announcements: Hooks, Suspense, Concurrent Mode and React Native’s new architecture.</p>
<p>The conference was taking place in Las Vegas. It had only one track with keynotes, talks and lighting talks. There were plenty of long breaks for attendees to chat.</p>
<p>All content was live streamed and the <a href="https://www.youtube.com/channel/UCz5vTaEhvh7dOHEyd1efcaQ/videos">video recordings</a> became available in just one day! (This is crazy, I’ve never seen the content from a conference appear online this fast).</p>
<p>In this post I’ve collected some thoughts about the talks. My favorites: <a href="https://www.youtube.com/watch?v=dpw9EHDh2bM">React Today and Tomorrow</a>, <a href="https://www.youtube.com/watch?v=Ew-UzGC8RqQ">Playing With Polyhedra</a>, <a href="https://www.youtube.com/watch?v=ByBPyMBTzM0">Concurrent Rendering in React</a>, <a href="https://www.youtube.com/watch?v=UcqRXTriUVI">React Native’s New Architecture</a> and <a href="https://www.youtube.com/watch?v=soAEB7ltQPk">Let React speak your language</a>.</p>
<p><img alt="" src="http://frantic.im/assets/react-conf-2018/og-image.jpg"></p>
<h2><a href="https://www.youtube.com/watch?v=dpw9EHDh2bM">React Today and Tomorrow</a></h2>
<p>In the keynote, Sophie talked about the state of React today and the most common problems: hard to reuse logic, wrapper hell and confusing classes.</p>
<p>Dan presented a proposed solution: Hooks. Hooks is a mechanism that allows functional components access more advanced features of React, like state, context or side effects. Here’s a quick example:</p>
<div><div><pre><code><span>import</span> <span>React</span><span>,</span> <span>{</span> <span>useState</span> <span>}</span> <span>from</span> <span>'react'</span><span>;</span>

<span>function</span> <span>Counter</span><span>()</span> <span>{</span>
  <span>const</span> <span>[</span><span>value</span><span>,</span> <span>setValue</span><span>]</span> <span>=</span> <span>useState</span><span>(</span><span>0</span><span>);</span>
  <span>return</span> <span>(</span>
    <span>&lt;</span><span>div</span><span>></span>
      <span>&lt;</span><span>span</span><span>></span><span>Value</span><span>:</span> <span>{</span><span>value</span><span>}</span><span>&lt;</span><span>/span</span><span>>
</span>      <span>&lt;</span><span>button</span> <span>onClick</span><span>=</span><span>{()</span> <span>=></span> <span>setValue</span><span>(</span><span>value</span> <span>+</span> <span>1</span><span>)}</span><span>></span>
        <span>Increment</span>
      <span>&lt;</span><span>/button</span><span>>
</span>    <span>&lt;</span><span>/div</span><span>>
</span>  <span>)</span>
<span>}</span>
</code></pre></div></div>
<p>Note that it’s still RFC and React team is collecting feedback. There is some interesting discussion on <a href="https://github.com/reactjs/rfcs/pull/68">the RFC pull request</a>.</p>
<p>I’ve had a chance to play with the new API and personally I think it’s <strong>AWESOME</strong>. Hooks do require a little bit of a mindset shift, but they make code a lot easier to write and understand.</p>
<p>Here’s a <a href="https://mobile.twitter.com/threepointone/status/1056594421079261185">clever illustration by @threepointone</a> that shows the difference between class-based components and functional components using hooks. Hooks make it easier to group and reuse behaviors that were spread out over multiple lifecycle methods.</p>
<p><img alt="" src="http://frantic.im/assets/react-conf-2018/class-vs-hooks.png"></p>
<h2><a href="https://www.youtube.com/watch?v=dpw9EHDh2bM">90% cleaner React</a></h2>
<p>Ryan Florence went into a more detailed demo of how hooks can be used to make code more readable and avoid common mistakes with effects. I would recommend watching it if you are still skeptical, otherwise it’s better to just get a feel of it in practice.</p>
<h2><a href="https://www.youtube.com/watch?v=kVSTKD13gos">Cloud-Only Dev Environment</a></h2>
<p>It’s interesting that a set of tools available online is more than enough to build a properly functioning app without setting up local development environment. Online IDEs + PaaS + static website hosting is enough to build fully-functional apps entirely from within your browser.</p>
<p>In this talk Christina built a simple TODO game in under 30 minutes only using a browser on her Chromebook (leveraging Cloud9, Firebase and Zeit Now).</p>
<h2><a href="https://www.youtube.com/watch?v=1e07uPWpvzI">Declarative Animations</a></h2>
<p>Animation is not a settled topic. We tend to swing between imperative and declarative. Both have pros and cons.</p>
<p>What I liked most from this talk is that the framework Matt described allows plugins and middleware. For example, a <code>Modal</code> component could describe what needs to be animated (opacity &amp; zoom) and a middleware could customize the properties of the spring: make it playful or subtle. This way the same component could be used on a wedding website or a site for kids, and the animation would “feel” different in both cases to match the mood.</p>
<p>I’m a little skeptical of animations that don’t take into account gestures. User input creates a lot of problems for animation libraries, and I’m not sure how the suggested framework can help.</p>
<h2><a href="https://www.youtube.com/watch?v=YSEUAi1dAdk">GraphQL without GraphQL</a></h2>
<blockquote>
<p>“REST is like asking for a pizza with all 30 toppings and throwing away the ones you don’t need”</p>
</blockquote>
<p>This walk was a little strange and fun and made audience chuckle a few times. The idea is to use GraphQL parser to extract queries from code, use them to send traditional REST request and then filter out fields that were not in the query.</p>
<h2><a href="https://www.youtube.com/watch?v=Ew-UzGC8RqQ">Playing With Polyhedra</a></h2>
<p>This talk was an unexpected gem! Nat Alison shared her lifetime passion for geometric figures and how solids are like Pokemon. Definitely worth a watch!</p>
<p>Check out the online <a href="https://polyhedra.tessera.li/">Polyhedra Viewer</a></p>
<h2><a href="https://www.youtube.com/watch?v=jd6FBBK1paA">Developing AR and VR Apps using React Native</a></h2>
<p>I’ve learned so much about our perception while working at Oculus. This talk has a few cool demos that are designed to trick your mind.</p>
<p>On the tech side, this is not about rendering every frame in JS. React helps build VR worlds by declaratively specifying what needs to be where, and all the heavy lifting is delegated to native frameworks.</p>
<h2><a href="https://www.youtube.com/watch?v=6La7jSCnYyk">React, JavaScript and WebAssembly To Port Legacy Native Apps</a></h2>
<p>Florian built <a href="https://gdevelop-app.com/">G-Develop</a> a decade ago. It’s a C++ app for making platformer games. In his talk he shows how he managed to convert the core of the app to JavaScript via asm.js and built a fresh UI on top of that with React.</p>
<p>Electron nowadays gets a lot of critique from developer community for its resource usage. Here’s an example of the value web technologies on desktop provide: the new G-Develop can run in your browser, gets more contributions, has a much faster development and release pace.</p>
<h2><a href="https://www.youtube.com/watch?v=tE-0xb2f44g">React For Social Change</a></h2>
<p>Making tech easier to use is not only about developer efficiency. It means more people can leverage tech to do more things, that in turn can make lives of millions of people better. I’m super proud of React Native :)</p>
<p>Rodrigo’s team built a React Native app for bus dispatchers and riders in Mexico City. It uses real-time data and ML to help everyone save time commuting.</p>
<h2><a href="https://www.youtube.com/watch?v=ByBPyMBTzM0">Concurrent Rendering in React</a></h2>
<p>This talk goes over two new features of React.</p>
<p>Suspense allows “pausing” the render until the data is ready.</p>
<p>Concurrent React is a new mode where React can interrupt rendering to serve high-priority update, or pre-render part of your app with a low priority.</p>
<p>Both features help solve the problems many developers don’t know they have. Check out Thomas Aylott’s quote at the end of this post, I think it’s spot on.</p>
<p>Oh, and don’t miss the part where Brian talks about the performance devtools. All the required pieces have finally came together and I’m excited about the new tooling.</p>
<h2><a href="https://www.youtube.com/watch?v=SCQgE4mTnjU">Moving To Suspense</a></h2>
<p>Ok, so now we have hooks, suspense and concurrent React. But we also have large React codebases that use none of these things yet.</p>
<p>Jared gives a step-by-step overview of how a team could approach the new React features. The good news is that you don’t have to rewrite all your code, nor you need to make risky library upgrades.</p>
<p>All new React features are opt-in and can be adopted incrementally.</p>
<h2><a href="https://www.youtube.com/watch?v=1gG8rtm-rq4">SVG illustrations as React Components</a></h2>
<p>SVGs are just XML-based documents containing vector image data. It’s easy to insert them directly into the DOM. In her (very first!) talk Elizabet shows how to use React to render SVG and the benefits of this approach.</p>
<p>Check out the <a href="https://github.com/miukimiu/react-kawaii">README of <code>react-kawaii</code></a> for a cool demo!</p>
<h2><a href="https://www.youtube.com/watch?v=qqffsEHKMcM">The Missing Abstraction of Charting</a></h2>
<p>My first though when I saw the title of the talk: “Yet another charting library”. I was very wrong!</p>
<p>Chris deeply researched the topic. Most libraries available for React provide high level API (e.g. <code>BarChart</code>) and sometimes access to very low level APIs (e.g. Canvas). All of them lack a mid-level abstraction. He took concepts from “The Grammar of Graphics” book and built a React-powered library that runs on web and mobile.</p>
<h2><a href="https://www.youtube.com/watch?v=eSwm1WZk7uA">Elsa</a></h2>
<p>Long time ago I’ve randomly discovered <a href="https://www.emacswiki.org/emacs/EmacsDoctor"><code>M-x doctor</code></a> and remember I was blown away. Unfortunately at the time I didn’t speak English much, but the Emacs Doctor did manage to surprise me a couple of times.</p>
<p>Mental health gets very little attention at conferences. We are all humans and our operating system isn’t free of bugs.</p>
<p>Damini shows how a bunch of cool technologies can be used to build a “debugger” with a conversational interface. Unfortunately the project isn’t available to play with yet.</p>
<h2><a href="https://www.youtube.com/watch?v=ZXqyaslyXUw">Block the Main Thread</a></h2>
<p>There’s one space where React Native isn’t great yet: making smooth user interactions. The problem is not JavaScript (which is actually reasonably fast), it’s the async bridge between JS and Native.</p>
<p>Fortunately, there are ways to make this better. James shows how one could dive into native side and build the required interactions using imperative native animations code. In the next talk Ram shows how the React Native team addresses the more generic issue.</p>
<p>For the record, here’s a photo of original React Native architecture as <a href="https://twitter.com/jordwalke">@jordwalke</a> explained it to me in 2014.</p>
<blockquote>
<p>“If the framework is async, making some parts of it synchronous would be easy. On the other hand, if the framework has all this assumptions of operation being synchronous, introducing async would be very hard”</p>
</blockquote>
<p><small>(see also: <a href="http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/">red and blue functions</a>)</small></p>
<p><img alt="" src="http://frantic.im/assets/react-conf-2018/react-arch.jpg"></p>
<h2><a href="https://www.youtube.com/watch?v=UcqRXTriUVI">React Native’s New Architecture</a></h2>
<p>Ram’s talk was really entertaining. It has a lot of visuals and animations that explain what’s going to happen to React Native internals in the near future.</p>
<p>I’m super excited about the future of React Native! The team is extremely talented, <a href="https://facebook.github.io/react-native/blog/2018/06/14/state-of-react-native-2018">the roadmap</a> is very solid with many things already being tested in production.</p>
<h2><a href="https://www.youtube.com/watch?v=soAEB7ltQPk">Let React speak your language</a></h2>
<p>Every single “JS localization” library I’ve seen in Open Source is terrible. It starts with something like this: define all strings your app is using in this giant XML/JSON/YAML/ini file.</p>
<p>If you don’t see why this approach sucks, <strong>this talk will blow your mind</strong>.</p>
<p><a href="https://lingui.js.org/index.html">LinguiJS</a>, the library Tomáš is working on, focuses on the right things:</p>
<ol>
<li>Developer experience. An easy-to-use API makes it trivial to mark things for translation. It’s also flexible enough to handle complex cases. It uses standard file formats that translation tools understand.</li>
<li>User experience. The library has a way to inline all translations into the JS files, making close-to-zero impact on the bundle size.</li>
</ol>
<p>Interestingly, at Facebook we use a similar library to translate our apps. I was hoping we could share it with the world, but unfortunately it’s too deeply integrated with the rest of build tooling.</p>
<h2><a href="https://www.youtube.com/watch?v=6DDdtt5zXPk&t=486s">React for Designers with FramerX</a></h2>
<p>Only half of the talk was about FramerX (which is a pretty cool piece of software). The other half was a little abstract and philosophical. If you don’t mind a little strange delivery format, there are some interesting thoughts.</p>
<p>My favorite quote:</p>
<blockquote>
<p>“The tools we’ve mastered influence our perception of what problems exist. If we only have certain tools (hammer) we are incapable of perceiving certain problems (screw)”</p>
</blockquote>
<h2><a href="https://www.youtube.com/watch?v=xYI9N0memLc">Building A Diverse And Inclusive Community</a></h2>
<p>It’s very hard. To make sure we build a community where everybody’s welcome, one needs to fight their built-in biases. For example, when organizing a meetup, it’s easy to not give enough thought to the food being ordered and miss people who have certain dietary restrictions.</p>
<p>Btw, as also noted by Eyitayo, React Conf was <em>AWESOME</em> at this. Organizers went above and beyond to make sure everybody’s welcome.</p>