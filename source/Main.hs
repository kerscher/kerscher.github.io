{-# LANGUAGE OverloadedStrings #-}
import Hakyll

configuration :: Configuration
configuration =
  defaultConfiguration
  { providerDirectory    = "source"
  , destinationDirectory = "docs"
  , storeDirectory       = "cache"
  , tmpDirectory         = "cache/tmp"
  }
  
main :: IO ()
main = hakyllWith configuration rules

rules :: Rules ()
rules = do
  match "templates/*" $ compile templateCompiler
  
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "style/*" $ do
    route   idRoute
    compile copyFileCompiler
  
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexContext =
            listField  "posts" postCtx (return posts) `mappend`
            constField "title" "Yghor Kerscher"       `mappend`
            constField "landingPage" mempty           `mappend`
            defaultContext           
      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
