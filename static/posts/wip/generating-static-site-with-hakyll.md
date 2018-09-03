## Generating a static site with Hakyll

A static site generator is not a trivial piece of software, there are many
things that you have to take in account in order to maximize user experience.
Despite this, there are plenty of options like Gatsby, Hugo, Jekyll and many
more. I decided to go with [Hakyll](https://jaspervdj.be/hakyll/) because I 
really like Haskell and it looked good enough for my use case. If you are not 
that into Haskell, you should probably look elsewhere. There's still some room 
for improvement, but I enjoyed using it and am thankful for its existence.

Installing Hakyll and building a new site is as simple as following [a
tutorial in the official site](https://jaspervdj.be/hakyll/tutorials/01-installation.html).
The only inconvenience is that it took 30 minutes for stack to download and 
build the `hakyll-init` executable, and halfway there all of my 4 CPUs were 
close to 100% usage.

I didn't change much of what was generated with `hakyll-init`. What I mostly did
was delete files I did not want like the "archive" or "about me" sections. I 
had to add a "js" directory for any  JavaScript files that I may need, and 
"fonts" dir with files that I downloaded from Google Fonts. I don't need Hakyll 
to process these files, just copy them  like it does with images. So, to copy 
all images, fonts and JS files I only had to do this:

``` Haskell
main :: IO ()
main = hakyll $ do
  -- More rules...
    match ("images/*" .||. "fonts/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler
```

Another thing that I needed was the ability to concatenate CSS files. I don't
use a lot of CSS as require a CSS preprocessor like SASS, but I do like to keep
my CSS files small and focused on a single thing. For example, the CSS used for
this article consists of two files: "post.css" and "like.css". One has the main
CSS for the page, and the other one has all the CSS needed for the like button
(I'll talk more about that later). I like to use two files for development, but
serving a single stylesheet has better performance because it reduces the number
of HTTP requests that the browser needs to send to the server in order to render
the page. The solution for this is to write different files and have Hakyll
concatenate them. To achieve that I use this simple template named "concat.txt":

```
 $for(items)$$body$$endfor$
```

This template just looks in its context for a list value named "items" and
concatenates all of the list items into a single text file. It even retains the
CSS minifactions done by the `compressCSSCompiler`. All there is left to
do is to load all the necessary CSS files and apply the template like this:

``` Haskell
main :: IO ()
main = hakyll $ do
    -- More rules...
    create ["css/post-bundle.css"] $ do
      route idRoute
      compile $ do
        cssFiles <- loadAll "css/posts/*" 
        let styleCtx = listField "items" defaultContext (return cssFiles)

        makeItem ""
            >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

```

"css/posts/main.css" and "css/posts/like.css" exist on my project, but 
"css/post-bundle.css" does not. To make Hakyll create new files I have to use
`create` instead of `match`. Then I can load all the stylesheets inside
"css/posts/" and apply the "concat.txt" template to create the actual
stylesheet that will be served.

One last thing that I wanted was to show previews or teasers of the post on the 
home page. Fortunately, Hakyll has good support for this, I just had to follow
[this tutorial](https://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html). 
After that I just needed to polish my CSS and look up some SVGs to use as icons.


