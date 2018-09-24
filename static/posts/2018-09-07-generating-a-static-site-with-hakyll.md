---
title: "Building a Blog Part 1: Generating a static site with Hakyll"
description: "Static sites with Haskell using the Hakyll library"
keywords: Haskell,Hakyll,CSS,fonts,JavaScript,blog,site
---

In part 1 of [Building a Blog](
./2018-09-03-is-this-finally-working-oh-hello-world.html) I will talk 
about how the static pages served in this blog are generated. Writing HTML 
for every posts is too low level and cumbersome. I don't need to rewrite the 
structure of the page for every post. It's better to write the content in a 
more high level language like markdown and convert it to HTML using a 
predefined template. Programs that are able to do this are usually called 
static site generators AKA just what I need. 

<!--more-->

A static site generator is not a trivial piece of software, there are many
things that you have to take in account in order to maximize user experience.
Despite this, there are plenty of options like Gatsby, Hugo, Jekyll and many
more. I decided to go with [Hakyll](https://jaspervdj.be/hakyll/) because I 
really like Haskell and it looked good enough for my use case. If you are not 
that into Haskell, you should probably look elsewhere. There's still some room 
for improvement, but I enjoyed using it and am thankful for its existence.

## Getting started

Installing Hakyll and building a new site is as simple as following [a tutorial 
in the official site](https://jaspervdj.be/hakyll/tutorials/01-installation.html).
The only inconvenience is that it took 30 minutes for stack to download and 
build the `hakyll-init` executable, and halfway there all of my 4 CPUs were 
close to 100% usage.

After running `hakyll-init` a scaffolded site is generated with pretty much 
everything you need for a typical blog. Rules for processing the different 
files in the project are declared with Haskell in the `site.hs` file. For
example this is how every single CSS file under `css/` is compressed:

``` Haskell
main :: IO ()
main = hakyll $ do
  -- More rules...
    match ( "css/*" .||. "css/**/*") $ do
        route   idRoute
        compile compressCssCompiler
```

The `route` function sets the route where the final file will be available on
the server. `route idRoute` simply means to use the same path that the original
file has in the file system. Functions that process files are named with the 
"Compiler" prefix and they are applied to the matched files with the `compile`
function. Images, fonts and JavaScript files don't need any processing so they 
can be directly copied with `copyFileCompiler`.

``` Haskell
main :: IO ()
main = hakyll $ do
  -- More rules...
    match ("images/*" .||. "fonts/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler
```

The most important compiler is `pandocCompiler`. It is the one that transforms 
the posts written in Markdown into valid HTML.

``` Haskell
main :: IO ()
main = hakyll $ do
  -- More rules...
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate 
                  "templates/post.html" postCtx
            >>= relativizeUrls
```

As you can see, the DSL is very elegant. Hakyll's official site has [a great 
tutorial for learning the basics of it](
https://jaspervdj.be/hakyll/tutorials/03-rules-routes-compilers.html). 
Documentation is pretty good, there are a few other articles with useful 
information like [how to display excerpts of posts on the index page](
https://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html).

## Syntax highlighting on code snippets

This blog is going to be mostly about programming so code snippets like the one 
above need syntax highlighting for better readability. The Pandoc compiler gets 
you halfway there, all you need to do is set the appropriate colors with CSS. I 
just copied the [default Pandoc syntax CSS file](
https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css) and changed 
the colors to somewhat resemble the [Tomorrow Theme](
https://github.com/chriskempson/tomorrow-theme).

## Managing CSS files

I don't use a lot of CSS as require a CSS preprocessor like SASS, but I do like 
to keep my CSS files small and focused on a single thing. For example, the CSS 
used for this article consists of four files: 

- `main.css` has most of the CSS used for the pages layout.
- `md.css` has CSS styles for elements rendered from markdown source.
- `syntax.css` has the colors used for syntax highlighting in code snippets. 
- `like.css` has the styles for the like button at the bottom (I'll talk more 
  about that later).

I like to use different files for development, but serving a single stylesheet 
has better performance because it reduces the number of HTTP requests that the 
browser needs to send to the server in order to render the page, so I needed 
some way of merging all these little files. The solution for this is to 
concatenate them with a Hakyll rule. To achieve that I use this simple 
template named `concat.txt`:

```
 $for(items)$$body$$endfor$
```

This template just looks in its context for a list value named "items" and
concatenates all of the list items into a single text file. It even retains the
CSS minifications done by the `compressCSSCompiler`. All there is left to
do is to load all the necessary CSS files and apply the template like this:

``` Haskell
main :: IO ()
main = hakyll $ do
    -- More rules...
    create ["css/post-bundle.css"] $ do
      route idRoute
      compile $ do
        cssFiles <- loadAll "css/posts/*" 
        let styleCtx = listField "items" 
                                 defaultContext 
                                 (return cssFiles)

        makeItem ""
            >>= loadAndApplyTemplate 
                  "templates/concat.txt" styleCtx

```

All the files that I listed above exist on my project, but 
`css/post-bundle.css` does not. To make Hakyll create new files I have to use
`create` instead of `match`. Then I can load all the stylesheets inside
`css/posts/` and apply the `concat.txt` template to create the actual
stylesheet that will be served.

## Lazy loading fonts

It's very easy to find pretty fonts on the internet and just dump them into 
your site. There is just one *little* problem: These files can be far 
bigger than your HTML or CSS, slowing down your load times. For example all 
the fonts used by this site have a combined size of 1.1 MB, whereas the 
average post's HTML and CSS has a combined size of roughly 10 KB. The 
fonts are 100 times bigger than the actual page. That's insane. 

How can I fix this? Well I don't know, I'm not an expert about the browser's
internals that render text loading the necessary fonts. So what I did was I
read [Monica Dinculescu's post about web fonts](
https://meowni.ca/posts/web-fonts/). She explains how the browser waits for the
necessary fonts to load before showing you the text and suggests to first render
text with fallback fonts until the actual fonts load asynchronously to prevent 
users from staring at blank screens for too long. 

After clearing that up the first step was to declare the font faces in a single 
CSS file that will be loaded asynchronously. So I created the file 
`css/font-faces.css` with this content:

``` CSS
@font-face {
  font-family: Merriweather;
  src: url("../fonts/Merriweather-Light.ttf");
  font-weight: 400;
}

@font-face {
  font-family: OpenSans;
  src: url("../fonts/OpenSans-Regular.ttf");
  font-weight: normal;
}

@font-face {
  font-family: OpenSans;
  src: url("../fonts/OpenSans-Bold.ttf");
  font-weight: bold;
}
```

Then I use XHR to load that stylesheet asynchronously so that it gets cached 
by the browser and only then add the `link` tag to the document's head. This is 
the JavaScript that I use:

``` JavaScript
  const fontfacesPath = "/css/font-faces.css"

  const setFontfacesStylesheet = () => {
    const newLink = document.createElement("link");
    newLink.rel = "stylesheet";
    newLink.href = fontfacesPath;

    document.head.appendChild(newLink);
  }

  export const linkFontFaces = () => {
    const xhr = new XMLHttpRequest();
    xhr.open('GET', fontfacesPath, true);
    xhr.onreadystatechange = function () {
      if (xhr.readyState == 4 && xhr.status == 200) {
        setFontfacesStylesheet();
      }
    };
    xhr.send();
  }
```

With this approach the browser renders the text ASAP with system fonts and only 
applies the custom fonts on a second render once they are loaded so that the 
user doesn't have to wait for fonts to load before reading an article. This is 
great for people with slow internet connections. Yay! However this isn't 
flawless. There are two slight inconveniences:

- If you are privileged enough to have a fast internet connection you will 
  notice how the browser quickly changes between fonts after the page loads. 
  It's not pretty, but hey, it's not a bug, it's a feature!
- If you disable JavaScript in your browser (and you probably should), will 
  only read articles using system fonts, but on the bright side you probably 
  don't care about it and are happy with all your pages loading super fast.

After lazy loading fonts, the static pages are ready to be served. The only 
thing that I have not mentioned yet is the "like" button at the bottom of this
page. It is implemented with JavaScript using [Mithril](
https://mithril.js.org/). Next part will be about a [Scotty](
https://github.com/scotty-web/scotty) web app that implements the endpoints 
that this button consumes and a later post will be about the widget's
implementation with Mithril and webpack.
