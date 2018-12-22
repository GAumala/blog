---
title: "Building a Blog Part 4: Using Nginx as a reverse proxy and static file server"
description: "Configuring Nginx as a static file server and reverse proxy."
keywords: Nginx,m4,tutorial,proxy,server
---

In part 4 of [Building a Blog](
./2018-09-03-is-this-finally-working-oh-hello-world.html) I will talk 
about Nginx and how it is configured to serve [static
files](./2018-09-07-generating-a-static-site-with-hakyll.html) efficiently and 
forward API requests to my [Scotty server](
./2018-09-12-creating-an-http-api-with-scotty-and-beam.html). The goal 
is to have Nginx as a secure and performant entry point to my site. 

<!--more-->

## Why Nginx

The web is wild. There are all kinds of web clients and specifications aren't 
followed 100% of the time. There are also malicious folks trying to bring your 
site down. In a production environment, it's better to have your application 
behind a robust and performant HTTP server like Nginx. It has every feature that
you could possibly need. The ones I'm most interested in are:

- Efficient static file serving
- Reverse proxy with caching
- Rate limiting

These things are essential for a performant site, but the thing I like the most 
about Nginx is how easy it is to setup [Let's Encrypt](
https://certbot.eff.org/lets-encrypt/ubuntuxenial-nginx). I think it's
very important to use HTTPS everywhere because: 

- Your interactions with a web site should be private. 
- No 3rd party should be able to inject ads or any extra content to a site you 
are visiting. 

Let's Encrypt's Certbot makes it really easy to use HTTPS everywhere in your 
server. It can even generate the necessary Nginx configuration and let you focus 
on other things. You can run any amount of apps and services on your server and
they all can be accessed with HTTPS via Nginx without having to patch them to
support HTTPS.

## Dealing with Configuration files

Nginx is another one of those programs has so many good features that you have to 
edit some files under `/etc` to get it to work for your use case. This is a
problem because ideally I want all the source files needed to deploy my blog
under a single repository. This way, if I ever need to deploy to a new server, 
all I have to is run `git clone` and run some scripts. Writing configuration
files is an extra step that I want to avoid.

A simple solution would be to create the configuration files inside the repo and
copy them to `/etc` during deploy. I think this is viable for most web apps,
when the Nginx instance is used exclusively for the web app. This isn't viable
for me because I'm hosting this on a Virtual Private Server (VPS) that I also
want to use to run other web apps and services. Those services should not be 
exposed to the internet directly, they should be behind Nginx (HTTPS 
everywhere!) and my blog's source code should not know anything about them. 

The solution here is to separate the global Nginx configuration, which knows
everything going on in the system, and the blog configuration. Lucky for me,
Nginx configuration language supports an `include` directive to load extra
config files from arbitrary configurations. My global Nginx configuration looks
something like this:

```
http {
  include /path/to/repo/nginx/http.conf

  # More configuration

  server {
    include /path/to/repo/nginx/server.conf

    # More configuration
  }
}
```

The next step would be to add the configuration files, but there is a little
problem. Those files contain some data that should be private or is only
relevant to the server where the blog is currently deployed. This data should
not be part of the repository. Instead, the repository should contain
configuration templates in which the server admin fills out the fields with the
private data.

In my case, I use [m4 (macro language)](
https://www.gnu.org/software/m4/m4.html) as my template engine. It doesn't seem
to be very popular among web developers, but it's available pretty much on every 
Linux distro and it's good enough for me. I found [this site](
https://mbreen.com/m4.html) to be very helpful to learn how to use it. I'm 
curious if there are better tools for this problem.

If you take a look into my blog's [config templates](
https://github.com/GAumala/blog/tree/master/nginx). You'll see that there are
only 3 variables:

- `API_PORT`: The port in which the blog's [backend](
./2018-09-12-creating-an-http-api-with-scotty-and-beam.html) listens.
- `SITE_PATH`: The location of the blog's static files (HTML, CSS & JS).  
- `CI-SECRET`: A secret string used by Circle CI to update the blog after a `git
  push`.

To generate the config files, all I have to do is define those variables when
invoking `m4` in the command line: 

``` bash
m4 -DAPI_PORT=9999 -DSITE_PATH=/path/to/_site/ -DCI_SECRET=6yhfdri-ed45-24on-5342-0b24q85m4452 ./server.m4 > ./server.conf

m4 ./http.m4 > ./http.conf

```
## Serving static files

The blog is mostly HTML and CSS files. I won't cache these files because they
are small, and I might need to correct them in case I publish a typo or
something like that. Serving these files is very simple.

```
root SITE_PATH;


location / {
    try_files $uri $uri/ =404;
}
```

First, set the directory containing the static files as the "server root". Then 
the `location /` block uses the path of any request received by the server and 
looks for a file that matches the path. If it can't find anything, it retries 
appending a `/`. If it still can't find a file to serve, it returns 404.

Some static files that are too big, or just don't ever need to be updated,
should be cached by clients to improve the site's performance. To inform
clients that these files should be cached I put them under `/assets` and use
this configuration:

```
location /assets {
        add_header Cache-Control "public, max-age=31536000, immutable";
        try_files $uri $uri/ =404;
}
```

This block adds a `Cache-Control` header to the response so that browsers can 
avoid re-downloading the file if it's already cached. I use `immutable` to declare 
that the file never changes so it should never download it more than once. If 
the browser does not support `immutable` it can fallback to `max-age=31536000`,
which tells the browser to cache the file for a ridiculously long time.

Another important thing when serving static files is GZIP compression. Text
files like HTML, CSS and JS can be reduced up to like a third of its original
size when using GZIP. This is a great way to save bandwidth. I enable this on my
global configuration because it is so good.

```
http {
  gzip on;
  gzip_min_length 1000;
  gzip_types      application/json application/xml image/svg+xml text/css text/plain;
}
```

Nginx has a lot more optimizations that for static files. My configuration
was originally created by `certbot` when I set up Let's Encrypt, so it already 
had some nice things like `tcp_nodelay`, `tcp_nopush`, and `sendfile` activated.
You can read more about those [in this article](
https://www.netguru.co/codestories/nginx-tutorial-performance).

## Reverse Proxy

My blog exposes an HTTP API to like blog posts using a [Scotty server that 
records every like button click into SQLite database](
./2018-09-12-creating-an-http-api-with-scotty-and-beam.html). SQLite is 
not really designed for web applications, but it's still pretty performant and
good enough for small sites like this one. I love the simplicity of an embedded 
database and not having to deal with another huge system like MySQL or Postgres. 
I don't expect to receive enough traffic to crash the system any time soon, but 
I still think that it would be irresponsible to expose this server directly to 
the internet. 

This is why I'm using Nginx as a reverse proxy. Nginx communicates clients using
SSL (provided by Let's Encrypt) and optionally, HTTP/2. If a client sends an API
request it is forwarded to the Scotty server using simpler protocols like
unencrypted HTTP/1.1. Here's the configuration for `/blogapi/like` which records 
a liked post when it receives a `POST` request:

```
location /blogapi/like {
        limit_req zone=post_like_limit burst=5 nodelay;

        rewrite ^/blogapi/like/(.*)$ /like/$1 break;

        proxy_set_header  X-Real-IP $remote_addr;
        proxy_pass http://localhost:API_PORT;
}
```

The requests are rate limited and rewritten before being forwarded to the Scotty
server listening on a different port on the same VPS. Rate limiting is super
important to prevent DoS attacks. Nginx has [a great blog post about rate 
limiting](https://www.nginx.com/blog/rate-limiting-nginx/) that I used as 
reference. The request is modified in two ways before being forwarded:

- The `rewrite` directive simply removes the `/blogapi/` prefix from the path, 
keeping any other path parameters intact. 
- The `proxy-set-header` directive adds an HTTP header to the proxied request 
with the original request's IP address so that the Scotty server can insert it 
to the database.

The configuration for `blogapi/likes` is a bit different because it returns the 
number of likes that a post has when it receives a `GET` request:

```
location /blogapi/likes {
        limit_req zone=api_limit burst=5 nodelay;

        rewrite ^/blogapi/likes/(.*)$ /likes/$1 break;

        proxy_pass http://localhost:API_PORT;
        proxy_cache blog;
        proxy_cache_valid 200 10s;
}
```

Just like the previous endpoint, the requests here are also rate limited and 
rewritten. There's no need to pass the IP address as a header since there is
nothing to insert. Since this is a `GET` request, the response is set to be
cached for a few seconds if the status code is 200. Caching is very useful
because it avoids repeated database queries on short intervals. Because of 
caching, this endpoint uses a more lenient rate limit "zone". 

That's it for today. I'm pretty happy with Nginx although maybe I could do just
fine with a simpler tool. The next post will be about continuous integration and
how I use [CircleCI](https://circleci.com/) to test my code and deploy the blog 
automatically after pushing to the master branch of my [GitHub repository](
https://github.com/GAumala/blog).
