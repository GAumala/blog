---
title: Is this finally working? Oh, hello world!
description: "Hello World!"
keywords: HTML,Web,programming,CSS,JavaScript,blog,site
---

I've been wanting to have my own blog for a while now, and only recently I
finally had the time to sit down and work on it. I think knowing how to write
and communicate effectively is a very valuable skill, specially for somebody 
working in tech. I'm by no means a good writer, but here I am. 

<!--more-->

There are many options on the internet for publishing whatever you want to write. 
Medium seems to be the most popular option these days, but personally I'd
prefer to stay away from closed cyberspaces because I believe in [the 
decentralized web](https://www.youtube.com/watch?v=izQFMADw70w) (That's a 
YouTube link, I know, the irony). Also, I like working with servers. I already
pay every month for a small VPS from Digital Ocean in which I have deployed 
a few self-hosted apps for my own private use. Managing a public website seemed 
like the next step, and a fun challenge as well, so I went for it. 

Before writing any code, you should always list your design goals. I thought
about my goals for a long time, and this is what I came up with:

1. **Accessibility.** Blog posts should render correctly and efficiently 
  everywhere. I do most of my reading on my phone, so I expect everything to be
  perfectly readable on small screens. A lot of people have slow internet 
  connections and/or low-end devices. I know, because I've been there. There's 
  no reason why a simple article should take more than few seconds to load. 
  You should not need to enable JavaScript either. Static pages should not rely 
  on JavaScript, it should only be used for little enhancements.
2. **No Tracking.** I value my own privacy and respect people's desire to not 
   be tracked on the internet. I don't need money from advertisers so I have no 
   use for third party trackers that slow down my site. I think it is cool to 
   have statistics about site usage, but such data should be gathered in a way
   that is anonymous, unobtrusive, and only if the user explicitly agreed to
   participate.
3. **Maintainable.** I will be maintaining this on my own free time, which is 
   *very* limited, so things should be as simple as possible. Everything should
   be automated, unless the task at hand is too ambiguous or dangerous for a 
   machine to do it. I shouldn't have to reinvent the wheel unless I'm having 
   fun. Publishing new posts or updating old ones should be as simple as 
   editing some markdown files and hitting `git push`.  

Living up to these goals was not a simple task at all. It involved using lots of
different technologies. My original plan was to document the whole process in
this post, but it ended up being way too big for a single post. So here's what
I'm gonna do: I will publish a series of articles in which I focus on a specific
technology that I used to build this blog. The series will consist of the
following 5 parts:

1. [Generating a static site with Hakyll](
    ./2018-09-07-generating-a-static-site-with-hakyll.html)
2. [Creating an HTTP API with Scotty](
    ./2018-09-12-creating-an-http-api-with-scotty-and-beam.html)
3. [Creating a like button widget with Mithril and webpack](
    ./2018-09-25-creating-a-like-button-with-mithril.html)
4. [Using Nginx as a reverse proxy and static file server](
    ./2018-12-22-using-nginx-as-a-reverse-proxy-and-static-file-server.html)
5. Continuous Integration with CircleCI

I'll update this list with the appropriate links after publishing every article.
All of my code is open source and you can find it [on GitHub](
https://github.com/GAumala/blog). I had a blast building this site, and I hope 
you find it useful. In the meantime, stay tuned!
