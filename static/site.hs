--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "fonts/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match ( "css/*" .||. "css/**/*") $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= relativizeUrls

    create ["css/post-bundle.css"] $ do
      route idRoute
      compile $ do
        cssFiles <- loadAll "css/posts/*" 
        let styleCtx = listField "items" defaultContext (return cssFiles)

        makeItem ""
            >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext