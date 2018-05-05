{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

import Hakyll

configuration :: Configuration
configuration = defaultConfiguration { providerDirectory    = "source"
                                     , destinationDirectory = "docs"
                                     , storeDirectory       = "cache"
                                     , tmpDirectory         = "cache/tmp"
                                     }

main :: IO ()
main = hakyllWith configuration $ do
  templates
  images
  style
  posts
  index

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

images :: Rules ()
images = match "images/*" $ do
  route idRoute
  compile copyFileCompiler

style :: Rules ()
style = match "style/*" $ do
  route idRoute
  compile copyFileCompiler

posts :: Rules ()
posts = match "posts/*" $ do
  route $ setExtension "html"
  compile
    $   pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

index :: Rules ()
index = match "index.html" $ do
  route idRoute
  compile $ do
    posts' <- recentFirst =<< loadAll "posts/*"
    let indexContext =
          listField "posts" postCtx (return posts')
            `mappend` constField "title"       "Yghor Kerscher"
            `mappend` constField "landingPage" mempty
            `mappend` defaultContext
    getResourceBody
      >>= applyAsTemplate indexContext
      >>= loadAndApplyTemplate "templates/default.html" indexContext
      >>= relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
