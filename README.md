# My Blog [![CircleCI](https://circleci.com/gh/GAumala/blog.svg?style=svg)](https://circleci.com/gh/GAumala/blog)

This is a monorepo with all the source code needed to build my blog. It is 
organized into 5 directories. Here's what each of them contains:

- `./static/` The static site built with Hakyll. It's mostly posts written in 
  Markdown, the templates used to transform them into HTML, and some stylesheets 
  and fonts.
- `./api/` A Scotty web app used as backend for the blog.
- `./frontend/` All the JS used by the blog, mostly built with ES6, Mithril 
  and bundled with webpack.
- `./nginx/` m4 files used to generate my Nginx config.
- `./.circleci/` my CircleCI configuration and other files used for building and
   deploying the blog with CircleCI jobs.

You can read more about this site here: https://gaumala.com/posts/2018-09-03-is-this-finally-working-oh-hello-world.html
