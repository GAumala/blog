---
title: "Building a Blog Part 3: Creating a Like button widget with Mithril and webpack"
description: "Web applications in Haskell with Scotty and Beam"
keywords: JavaScript,ES6,Babel,webpack,Mithril,widget,button
---

In part 3 of [Building a Blog](
./2018-09-03-is-this-finally-working-oh-hello-world.html) I will talk about
the implementation of the like button at the bottom of every post in this 
website. The goal is to have an interactive widget that lets users see the 
number of likes for the post they are currently reading and optionally increment 
it by one unit. It is written with [Mithril](https://mithril.js.org/), a modern 
JavaScript framework for fast [Single Page Applications](
https://en.wikipedia.org/wiki/Single-page_application).

<!--more-->

## Why a JavaScript framework?

For a like button, A `button` inside a `form` certainly gets the job done with 
zero JavaScript, but there is a little problem. The page reloads every time the 
form submits a POST request, which is not the best user experience in this case. 
The better approach is to use [AJAX](
https://developer.mozilla.org/en-US/docs/Web/Guide/AJAX) so we are going to need 
at least a little JavaScript. On top of that, the button has a state, it is 
either pressed (you liked the post) or it isn't. The button's visual appearance
and behavior changes depending on the state. Managing state is not a simple task
so it's better to use a tool that is designed to efficiently update the UI every
time state changes.

## Why Mithril?

From [Mithril's homepage](https://mithril.js.org/):

> Mithril is a modern client-side Javascript framework for building Single Page
Applications. It's small (< 8kb gzip), fast and provides routing and XHR 
utilities out of the box.

As you can see, this fits perfectly in my blog since one of my goals is to keep
the amount of JavaScript to a minimum to ensure faster page loading times. Doing
an interactive button in plain old JavaScript is not something I consider
maintainable because of the pain of dealing with state management. That's the 
main reason why I decided to go with Mithril. 

Whenever an event is triggered, Mithril automatically detects which parts of 
the UI need an update and efficiently re-renders those only. This same idea is 
implemented slightly differently by all popular front-end frameworks like React,
Vue, Elm and many others. These frameworks have become so mainstream lately 
because they free you from having to write imperative code that manually calls 
DOM APIs to update the UI everytime somthing happens in your app. 

I usually go with React for front-end applications, because it's a great 
framework with a fantastic ecosystem, but for now I only want a small button. 
React is way too big for that. Mithril only adds 8 kB to the gzipped bundle 
size, instead of React's 30+ kB. Also, inside those 8 kB there's also a XHR 
utility included which helps with the AJAX calls. No need to add an extra 
library. On hindsight, [Preact](https://preactjs.com/) would have probably 
been a better choice since it is only 3 kB and I don't need Mithril's routing. 
I will consider it for future  projects, but I'm happy with Mithril so it stays
in my blog.


## Getting Started

Like with most front-end development in 2018, the first step is to write some
configuration files. This isn't strictly necessary to get started with Mithril,
but there are 2 things that I want that aren't included out of the box:

- Transpilation of ECMAScript 6+ features and [JSX](https://reactjs.org/docs/introducing-jsx.html) 
  to ECMAScript 5, which is the JavaScript version that modern browsers can
  run without issues.
- A development server that reloads pages everytime a source file changes.

These things only improve the development experience. They don't affect
performance or user experience in any way, so if you are not interested you can
skip this section.

Since I'm going to need [Babel](https://babeljs.io/) and [webpack](
https://webpack.js.org/), it is necessary to install a bunch of packages from 
the NPM registry. This can be achieved with [yarn](https://yarnpkg.com/en/):

```
yarn add --dev babel-core babel-loader babel-plugin-transform-react-jsx babel-preset-env webpack webpack-cli webpack-dev-server
```

My `package.json` file ends up like this:

``` json
{
  "devDependencies": {
    "babel-core": "^6.26.3",
    "babel-loader": "^7.1.5",
    "babel-plugin-transform-react-jsx": "^6.24.1",
    "babel-preset-env": "^1.7.0",
    "webpack": "^4.16.5",
    "webpack-cli": "^3.1.0",
    "webpack-dev-server": "^3.1.5"
  }
}
```

Babel is the compiler that transforms whatever flavor of JavaScript you want to
use into something that browsers can actually run. It is super extensible. 
First, you add `babel-core`, which contains the base compiler, and then you add
a preset and some plugins that know how to transpile the syntax that you want to
use into valid ES5. In this case I add `babel-preset-env`, which is a preset 
that transpiles ES6+ code, and `babel-plugin-transform-react-jsx`, which is a 
little plugin that transpiles JSX to components for any supported framework. 
To tell Babel to use these packages, I have the following `.babelrc` 
configuration file:

``` JSON
{
    "presets": ["env"],
    "plugins": [
        ["transform-react-jsx", {
            "pragma": "m"
        }]
    ]
}
```

Transpilation by itself isn't terribly useful in production environments. To 
optimize the page's loading time, the generated code must be bundled into a 
single minified JS file that can be quickly sent over the network. This is 
where `webpack` comes in. Webpack is also very extensible. It can bundle pretty 
much any kind of file that you could possibly want to include in your web app. 
All you have to do is install the appropriate "loader" and use it in your config 
file. In this case I wanted webpack to transform my ES6 files with Babel before 
bundling them so I chose `babel-loader`.  Since I'm going to be using webpack 
from a terminal, I also need `webpack-cli`. The last dependency is
`webpack-dev-server`. This is used by webpack to spin up a small web server that
serves the bundled JS and reloads the browser every time a source file changes
and a new bundle is generated. My `webpack.config.js` file ends up being like
this:

``` JavaScript
const path = require('path')

module.exports = {
  entry: {
    home: './src/home.index.js',
    post: './src/post.index.js'
  },
  output: {
    path: path.resolve(
      __dirname, 
      './output/js'
    ),
    filename: '[name].js',
  },
  module: {
    rules: [{
      test: /\.js$/,
      exclude: /node_modules/,
      loader: 'babel-loader'
    }]
  },
  devServer: {
    contentBase: path.resolve(
      __dirname, 
      '../static/_site'
    ),
    publicPath: '/js/',
    compress: true,
    port: 9000,
    proxy: {
      '/blogapi': {
        target: "http://localhost:8008",
        headers: { 
          "X-real-ip": "0.0.0.0" 
        },
        pathRewrite: {
          '^/blogapi' : ''
        }
      }
    }
  }
}
```

This file specifies several things so I'll try to explain them one by one. 

* The `entry` section declares two entry points because there will be two bundles
that will share some code: one for the posts page and another for the home page.
* The `output` section declares that these two bundles will be generated at
`output/js/post.js` and `output/js/home.js` respectively. 
* The `module` section adds a rule that says that every JS file must be
  transformed with Babel  before bundling.
* The `devServer` section configures the development server that I talked
about earlier. It listens on port 9000 and serves all generated bundles as if 
they were actual files mounted and `/js/`. In addition to bundles, it also
serves all the files in `../static/_site` which is [where Hakyll outputs my 
static pages](./2018-09-07-generating-a-static-site-with-hakyll.html). This 
allows me to try out my JS code on the actual pages of the blog. Finally, 
it proxies all `/blogapi` HTTP requests to [my API Server](
./2018-09-12-creating-an-http-api-with-scotty-and-beam.html), rewriting the 
path and adding extra headers so that they can be processed correctly 
just like if they had been forwarded by Nginx in production. This lets me verify
that the button is using the endpoints correctly.

Now that the tools are properly configured all that's left to do is to actually
run them. In development, webpack should start the dev server and generate
bundles as quickly as possible, without any optimizations, and include source
maps so that they can debugged. Luckily, the [`-d` flag](
https://webpack.js.org/api/cli/#shortcuts) is a shortcut that does just that:

```
yarn webpack-dev-server -d
```

For production, the bundle should be minified and optimized. The [`-p` flag](
https://webpack.js.org/api/cli/#shortcuts) is another nice shortcut that gets 
the job done:


```
yarn webpack -p
```

## Writing the widget

Once configuration is done, I can actually write the code for my widget. It
consumes the endpoints created in part 2, so let's add the functions for the
AJAX calls:

``` JavaScript
import m from "mithril";
import {postStringId} from "./browser.js";

export const incrementLikesCount = () =>  {
  return m.request({
    method: "POST",
    url: `/blogapi/like/${postStringId}`
  })
}

export const getLikesCount = () => 
  m.request({
    method: "GET",
    url: `/blogapi/likes/${postStringId}`
  })
```

`postStringId` is just a helper function that extracts the post's ID from the
URL. These 2 functions use Mithril's [`request` function](
https://mithril.js.org/#xhr) which returns [a promise](
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) 
that resolves to the server response without the need for a promise polyfill. As
you can see, it is very easy to do asynchronous HTTP requests with Mithril.

The widget will be handled by the `app` function. The idea is to display 3 
things: the button, a counter with the post's number of likes and a little 
message to inform the user about the button. The first thing that I will 
do here is to declare my state.

``` JavaScript
const app = () => {
  let liked = false;
  let likesCount = "??";
  let lastCountFromServer = likesCount;
  let footerMsg = "If you liked this post, please hit this like button to show your appreciation.";

  // rest of the function...
}
```

The first variable `liked` is self-explanatory, the post is either liked or not.
The widget will be gray at start but red after being liked. `likesCount` is the
"number" of likes to show in the counter. It starts with "??" because this 
value has to be fetched via AJAX, so it is unknown until the response arrives.
`lasCountFromServer` holds the last value of `likesCount`. This is handy for 
"rollbacks" whenever an error is thrown. Finally `footerMsg` is a message to 
display next to the button. It first invites the user to like the post and when
an error occurs this can be used to show an error message. 

Now that state is declared let's add some functions to manipulate it.

``` JavaScript
const app = () => {
  let liked = false;
  let likesCount = "??";
  let lastCountFromServer = likesCount;
  let footerMsg = "If you liked this post, please hit this like button to show your appreciation.";

  const updateLikesCounter = newValue => { 
    likesCount = newValue; 
    lastCountFromServer = newValue; 
  }

  const undoLike = () => {
    liked = false;
    likesCount = lastCountFromServer;  
    footerMsg = "Oops! Something went wrong. Please try again in a few seconds."
  }

  const onCreateLikeButton = () =>
    getLikesCount().then(updateLikesCounter, () => {})

  const onLikeButtonClicked = () => { 
    liked = true; 
    if (typeof likesCount == "number") 
      likesCount += 1;

    incrementLikesCount()
      .then(newValue => {
        updateLikesCounter(newValue);
        footerMsg = "Thank you!"
      })
      .catch(undoLike);
  }

  // rest of the function...
}
```

`onCreateLikeButton` and `onLikeButtonCliked` are the only functions that get
called by the component, the other ones are just helpers. 

When the component is created, `onCreateLikeButton` gets called and it sends an
HTTP request to get the current like count for that post from the server. If 
the request succeeds, both `likesCount` and `lastCountFromServer` get updated 
with the new value. If the request fails then nothing is done. 

When the button is clicked, then the button sends the HTTP request to update 
the likes counter in the server. The button is optimistic, it assumes that the 
request will succeed so it immediately sets `liked` to true and tries to update
the in-memory counter. If the request succeeds then the counter is updated to
whatever value the server currently has and sets `footerMsg` to a thank you 
message. If it fails, then the button has to rollback the optimistic update and
set `footerMsg` to an error message. This is the whole point of keeping 
`lastCountFromServer`, so that the component can quickly return to that value 
when things go wrong.

Notice how there is no special API to update the component's state like React's
`setState`. After every event, Mithril runs the callbacks and then figures out
which parts in the UI actually changed and re-renders them.

The only thing left to do in the `app` function is to return a component. Every 
component must expose a `view` method and optionally other functions for 
lifecycle events or *hooks*. In this case I only need the `oncreate` hook to 
load the post's like count so this is what `app` returns:

``` JavaScript
return ({
    oncreate: onCreateButton,
    view: () => 
      <table className="like-footer">
        <tr>
          <td className="like-me">
            <LikeCounter liked={liked} likesCount={likesCount}/>
            <LikeButton liked={liked} 
              onLikeButtonClicked={liked ? null : onLikeButtonClicked}/>
          </td>
          <td className="pls-like">
            <em>{footerMsg}</em>
          </td>
        </tr>
      </table>
  })
```

JSX can be used in the `view` function so that it can resemble the HTML that is 
generated on the browser. This function just returns a table with one row and two columns. The button and counter go on the first column and the message contained in `footerMsg` goes in the second column. The state variables `liked` and `likesCount` are
passed to the smaller components `LikeButton` and `LikeCounter` as attributes. 
Notice that the `onLikeButtonClicked` callback is only set if the button is not
already liked to avoid unnecessary actions. Here's the code for the rest of the
components:

``` JavaScript
const LikeCounter = {
  view: ({ attrs: { liked, likesCount } }) => 
    <div id="like-count" className={liked ? "liked" : ""}>
      {likesCount}
    </div>
}

const LikeButton = {
  view: ({ attrs: { liked, onLikeButtonClicked } }) => 
    <div id="heart" 
      style={liked ? {backgroundPosition: "right"} : null} 
      className={liked ? "heart-animation" : ""}
      onclick={onLikeButtonClicked}>
    </div> 
}
```

These two take the data passed by the parent from the `attrs` object in 
`view`'s only argument and use it to render the DOM elements accordingly. 
Finally, to add the widget to the page, the `app` function must be mounted:

``` JavaScript
  const e = document.getElementById("my-like-widget")
  m.mount(e, app);
```

And that's it! There is no need to mess with DOM APIs like `createElement`, 
`findElementById`, `innerText`, or `innerHTML`, the framework takes care of all
that. After creating the production bundle with webpack, the widget is ready 
to be served. For that matter I use Nginx. Next part will be about configuring 
Nginx to serve static files efficiently and proxy API requests correctly.
