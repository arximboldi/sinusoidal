{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified GHC.IO.Encoding as Encoding

--
-- main hakyll generation script
--
main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8
  hakyll $ do
    match "pic/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/main*.scss" $ do
      route   $ setExtension "css"
      compile scssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match (fromList ["site/about.rst",
                     "site/contact.markdown"]) $ do
        route   $ site $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "site/posts/*" $ do
        route $ site $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "site/posts.html" $ do
        route   $ constRoute "posts/index.html"
        compile $ do
            posts <- recentFirst =<< loadAll "site/posts/*"
            let ctx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/base.html" ctx
                >>= relativizeUrls

    match "site/index.html" $ do
        route $ site idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/base.html" defaultContext
                >>= relativizeUrls

--
-- modify routes for site
--
site :: Routes -> Routes
site = composeRoutes $ gsubRoute "site/" (const "")

--
-- generate date field
--
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- create a SCSS compiler that transpiles the SCSS to CSS and minifies
-- it (relying on the external 'sass' tool)
-- https://codetalk.io/posts/2016-05-10-compiling-scss-and-js-in-hakyll.html
scssCompiler :: Compiler (Item String)
scssCompiler = do
  getResourceString
    >>= withItemBody (unixFilter "sass" [ "-s"
                                        , "--scss"
                                        , "-I", "css"
                                        ])
