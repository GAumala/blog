## Creating a Like button widget with Mithril and webpack

Now that API is ready, what is left to do is to add to the post pages a like 
button that consumes these endpoints. This will be done via AJAX, so JavaScript
is required. One of my goals is to keep the amount of JavaScript to a minimum
and support all browsers. Doing an interactive button in plain old JavaScript is
not something I consider mantainable so I went with [Mithril](
https://mithril.js.org/) and [webpack](https://webpack.js.org) for bundling. I
usually go with React for frontend applications, but I only want a small like
button. React is way too big for that. Mithril, only adds 8 kb to the gzipped
bundle size, so it should help with page load times. Also, inside those 8 kb
there's also a XHR utility included which helps with the AJAX calls.



