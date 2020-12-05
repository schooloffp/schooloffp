---
layout: page
title: Blog
permalink: /blog/
---


<ul class="no-bullet">
  {% for post in site.posts %}
    <li>
      <h2><a href="{{ post.url }}">{{ post.title }}</a></h2>
      {{ post.excerpt }}
      <br/>
      <hr/>
      <br/>
    </li>
  {% endfor %}
</ul>
