---
title: "Kafka Everywhere: New Plans and Pricing for Apache Kafka on Heroku"
date: 2017-09-14
origin: 
alternate: http://feedproxy.google.com/~r/heroku/~3/L7hbqbV_ArE/kafka-on-heroku-new-plans
type: link
---

<p>Event-driven architectures are on the rise, in response to fast-moving data and constellations of inter-connected systems. In order to support this trend, last year we released <a href="https://www.heroku.com/kafka">Apache Kafka on Heroku</a> - a gracefully integrated, fully managed, and carefully optimized element of Heroku's platform that is the culmination of years of experience of running many hundreds of Kafka clusters in production and contributing code to the Kafka ecosystem.</p>
<p>Today, we are excited to announce additional plans and pricing in our Kafka offering in order to make Apache Kafka more accessible, and to better support development, testing, and low volume production needs. </p>

<h2>
<a name="apache-kafka-on-heroku-now-with-more-flexibility-and-speed" href="https://blog.heroku.com/kafka-on-heroku-new-plans#apache-kafka-on-heroku-now-with-more-flexibility-and-speed">Apache Kafka on Heroku: Now With More Flexibility and Speed</a>
</h2>
<p><a href="https://kafka.apache.org/">Apache Kafka</a> is a powerful, distributed streaming platform, and the dominant open source solution in managing high scale event streams. Kafka enables you to easily design and implement architectures for many important use cases, such as elastic queuing, data pipelines &amp; analytics, and microservices coordination. Apache Kafka on Heroku removes the complexity and cost of running Kafka, making its valuable resources available to a broad range of developers and applications. </p>
<p>The new addition to our managed Kafka plans, <strong>Basic,</strong> is based on a robust multi-tenant Kafka architecture. Multi-tenancy provides for much faster access to Kafka, with new resources being available in a matter of seconds, rather than minutes, and a much more accessible price point. This allows us to better serve application creators needing Kafka configurations that are more suitable for development or staging, or as production environments that don't require the full capacity of a dedicated cluster (as provided in our existing <strong>Standard</strong> and <strong>Extended</strong> plans).</p>
<table>
<thead>
<tr>
<th></th>
<th><strong>Basic</strong></th>
<th><strong>Standard</strong></th>
<th><strong>Extended</strong></th>
</tr>
</thead>
<tbody>
<tr>
<td><strong>Cluster</strong></td>
<td>Shared</td>
<td>Dedicated</td>
<td>Dedicated</td>
</tr>
<tr>
<td><strong>Event Stream Volume</strong></td>
<td>Low</td>
<td>High</td>
<td>Massive</td>
</tr>
<tr>
<td><strong>Ideal Use Case</strong></td>
<td>Develop + Test, <br>Low-Scale Production</td>
<td>Production</td>
<td>Production</td>
</tr>
<tr>
<td><strong>Price ($/month)</strong></td>
<td>$100+</td>
<td>$1,000+</td>
<td>$4,000+</td>
</tr>
</tbody>
</table>
<p></p>
<p>These new multi-tenant plans allow for rapid creation of Kafka add-ons, easy integration into Heroku applications, and the world-class monitoring and tooling you have come to expect from Heroku.</p>
<iframe allowfullscreen src="https://player.vimeo.com/video/233739649" width="640" height="230"></iframe>
<h2>
<a name="get-started-today" href="https://blog.heroku.com/kafka-on-heroku-new-plans#get-started-today">Get Started Today</a>
</h2>
<p>Using one of the new plans is as simple as provisioning an instance of the Kafka add-on and attaching it to a Heroku app:</p>
<pre><code>heroku addons:create heroku-kafka:basic-0 -a sushi-app
</code></pre>
<p>We are excited to see what you build with Kafka! Full details of the new Kafka plans can be found in <a href="https://elements.heroku.com/addons/heroku-kafka">Heroku Elements</a> and in <a href="https://devcenter.heroku.com/articles/multi-tenant-kafka-on-heroku">Heroku Dev Center</a>. If you have any questions or feedback, please let us know at <a href="mailto:kafka@heroku.com">kafka@heroku.com</a>.</p><img width="1" alt="" src="http://feeds.feedburner.com/~r/heroku/~4/L7hbqbV_ArE" height="1">