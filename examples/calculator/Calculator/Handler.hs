{-|
Module      : Calculator.Handler
Description : The request handling for the calculator kernel.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module implements the main request handler (ClientRequestHandler) for the demo Calculator
kernel that comes with the `jupyter` package.

The Calculator kernel implements a very simple language, represented by the following AST:

@
data Expr = Lit Int
          | Add Expr Expr
          | Multiply Expr Expr
          | Subtract Expr Expr
          | Divide Expr Expr
          | Negate Expr
          | Var Char

data Statement = Compute [(Char, Int)] Expr
               | Print Expr
@

Expressions in our calculator are represented by an @Expr@, and the things calculator the
calculator can do with the expressions are the constructors of @Statement@:
  * @Compute@: Given a mapping from variables to values, compute and print the value of the expression.
  * @Print@: Print a representation of the expression (emits both plain text and LaTeX).

The kernel features implemented by this kernel include code execution, autocompletion of 
constructor names, and inspection of constructor names. To simplify the code, parsing is omitted,
and instead the entire language syntax is simply Haskell expressions, so that we can use the
derived Read parsers.

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Calculator.Handler (requestHandler) where

import           Data.List (nub)
import           Text.Read (readMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Control.Concurrent (MVar, modifyMVar)

import           Jupyter.Kernel (defaultClientRequestHandler, KernelProfile, KernelCallbacks(..))
import           Jupyter.Messages (ClientRequest(..), KernelReply(..), KernelInfo(..),
                                   LanguageInfo(..), HelpLink(..), CodeBlock(..), CodeOffset(..),
                                   ExecutionCount, KernelOutput(..), ErrorInfo(..), displayPlain,
                                   displayLatex, CompletionMatch(..), CursorRange(..),
                                   pattern CompleteOk, pattern ExecuteOk, pattern InspectOk,
                                   pattern ExecuteError)

-- | An expression in our simple calculator language. Values are all integers, but variables are
-- represented by a single character name @Var@ constructor.
data Expr = Lit Int
          | Add Expr Expr
          | Multiply Expr Expr
          | Subtract Expr Expr
          | Divide Expr Expr
          | Negate Expr
          | Var Char
  deriving (Eq, Ord, Show, Read)

-- | Statements representing the things we can do with our expressions.
data Statement = Compute [(Char, Int)] Expr -- ^ Compute the value of an expression, substituting
                                            -- variables as necessary. If variables are left over
                                            -- after substitution, an error is raised.
               | Print Expr  -- ^ Print a mathematical representation of the expression.
  deriving (Eq, Ord, Show, Read)

-- | Parse a 'Statement' in our language by using its Read instance.
parse :: Text -> Maybe Statement
parse = readMaybe . T.unpack

-- | Evaluate an expression, substituting in variables as necessary. If any variables are
-- unevaluated or division by zero occurs, yields Nothing.
eval :: [(Char, Int)] -> Expr -> Maybe Int
eval vars expr =
  case expr of
    Lit i -> return i
    Add a b -> (+) <$> eval vars a <*> eval vars b
    Multiply a b -> (*) <$> eval vars a <*> eval vars b
    Subtract a b -> (-) <$> eval vars a <*> eval vars b
    Divide a b -> do
      denom <- eval vars b
      if denom == 0
        then Nothing
        else div <$> eval vars a <*> pure denom
    Negate a -> negate <$> eval vars a
    Var c -> lookup c vars

-- | Print an expression as an ASCII string.
--
-- Do not bother with clean parentheses, this is just a demo!
printText :: Expr -> String
printText expr =
  case expr of
    Lit i        -> show i
    Var c        -> [c]
    Negate e     -> '-' : printText e
    Add a b      -> concat ["(", printText a, " + ", printText b, ")"]
    Multiply a b -> concat ["(", printText a, " * ", printText b, ")"]
    Subtract a b -> concat ["(", printText a, " - ", printText b, ")"]
    Divide a b   -> concat ["(", printText a, " / ", printText b, ")"]

-- | Print an expression as a LaTeX string.
printLatex :: Expr -> String
printLatex expr =
  case expr of
    Lit i        -> show i
    Var c        -> [c]
    Negate e     -> '-' : printLatex e
    Add a b      -> concat ["(", printLatex a, " + ", printLatex b, ")"]
    Multiply a b -> concat ["(", printLatex a, " \\cdot ", printLatex b, ")"]
    Subtract a b -> concat ["(", printLatex a, " - ", printLatex b, ")"]
    Divide a b   -> concat ["\\frac{", printLatex a, "}{", printLatex b, "}"]

-- | List of symbols that should be part of autocompletions.
autocompleteSymbols :: [Text]
autocompleteSymbols = map fst tokenDocumentation

-- | Documentation for symbols in our language, stored as an association list.
tokenDocumentation :: [(Text, Text)]
tokenDocumentation =
  [ ("Lit", "Lit: Create an integer literal.")
  , ("Var", "Var: Create a variable with a single character name.")
  , ("Negate", "Negate: Negate an expression.")
  , ("Add", "Add: Add two expressions.")
  , ("Multiply", "Multiply: Multiply two expressions.")
  , ("Subtract", "Subtract: Subtract one expression from another.")
  , ("Divide", "Divide: Divide an expression by another.")
  , ("Compute", "Compute: Given an expression and variable bindings, compute the expression value.")
  , ("Print", "Print: Print an expression as text or LaTeX.")
  ]

-- | The main request handler for the Calculator kernel.
--
-- The request handler is responsible for generating a KernelReply when given a ClientRequest, optionally
-- publishing results using the callbacks provided to it in the 'KernelCallbacks' record.
requestHandler :: KernelProfile -> MVar ExecutionCount -> KernelCallbacks -> ClientRequest -> IO KernelReply
requestHandler profile execCountVar callbacks req =
  case req of
    ExecuteRequest code _ ->
      -- For this simple kernel, ignore the execution options, as they do not apply
      -- to our simple kernel. Also, automatically increment the execution counter.
      modifyMVar execCountVar $ \execCount -> do
        sendKernelOutput callbacks $ ExecuteInputOutput execCount code
        reply <- handleExecuteRequest execCount code callbacks
        return (execCount + 1, reply)
    InspectRequest code offset _ ->
      -- Ignore the detail level, because for this simple kernel we don't have
      -- documentation of identifiers at multiple detail levels.
      handleInspectRequest code offset
    CompleteRequest code offset -> handleCompleteRequest code offset
    other ->
      -- Any unhandled messages can be handled in the default manner.
      defaultClientRequestHandler profile kernelInfo callbacks other
  where
    -- This KernelInfo is returned by the default client request handler when it receives a
    -- KernelInfoRequest message, which is usually the first message that the client sends to the
    -- kernel.
    kernelInfo = KernelInfo
      { kernelProtocolVersion = "5.0"
      , kernelBanner = "Welcome to the Haskell Calculator Test Kernel!"
      , kernelImplementation = "Calculator-Kernel"
      , kernelImplementationVersion = "1.0"
      , kernelHelpLinks = [ HelpLink "jupyter package doc"
                              "http://github.com/gibiansky/jupyter-haskell"
                          ]
      , kernelLanguageInfo = LanguageInfo
        { languageName = "calculator"
        , languageVersion = "1.0"
        , languageMimetype = "text/plain"
        , languageFileExtension = ".txt"
        , languagePygmentsLexer = Nothing
        , languageCodeMirrorMode = Nothing
        , languageNbconvertExporter = Nothing
        }
      }

findPreceedingToken :: Text -> Int -> Text
findPreceedingToken code offset =
  let beforeCursor = T.take offset code
      allowedSymbolChars = nub $ T.unpack $ T.concat autocompleteSymbols
      token = T.takeWhileEnd (`elem` allowedSymbolChars) beforeCursor
  in token

handleExecuteRequest :: ExecutionCount -> CodeBlock -> KernelCallbacks -> IO KernelReply
handleExecuteRequest execCount (CodeBlock code) KernelCallbacks { .. } =
  case parse code of
    Nothing -> do
      -- Parse error!
      let errMsg = "Could not parse: '" <> code <> "'"
      sendKernelOutput $ DisplayDataOutput $ displayPlain errMsg
      reply $ ExecuteError
                ErrorInfo { errorName = "Parse Error", errorValue = errMsg, errorTraceback = [] }
    Just (Compute bindings expr) ->
      case eval bindings expr of
        Nothing -> do
          let errMsg = "Missing variable bindings in: '" <> code <> "'"
          sendKernelOutput $ DisplayDataOutput $ displayPlain errMsg
          reply $ ExecuteError
                    ErrorInfo { errorName = "Eval Error", errorValue = errMsg, errorTraceback = [] }
        Just val -> do
          sendKernelOutput $ DisplayDataOutput $ displayPlain $ T.pack $ show val
          reply ExecuteOk

    Just (Print expr) -> do
      let text = T.pack $ printText expr
          latex = T.pack $ printLatex expr
      sendKernelOutput $ DisplayDataOutput $ displayPlain text <> displayLatex latex
      reply ExecuteOk
  where
    reply = return . ExecuteReply execCount

handleInspectRequest :: CodeBlock -> CodeOffset -> IO KernelReply
handleInspectRequest (CodeBlock code) (CodeOffset offset) =
  let token = findPreceedingToken code offset
  in return . InspectReply . InspectOk $ displayPlain <$> lookup token tokenDocumentation

-- | Generate autocompletions for the symbols used in our language.
--
-- The algorithm for autocompleting is very simple: find the preceeding token by looking at which
-- characters are allowed in symbols, then search through all available symbols to find which ones
-- start with the found token.
handleCompleteRequest :: CodeBlock -> CodeOffset -> IO KernelReply
handleCompleteRequest (CodeBlock code) (CodeOffset offset) =
  let token = findPreceedingToken code offset
      start = offset - T.length token
      completions = filter (T.isPrefixOf token) autocompleteSymbols
  in return $ CompleteReply $
    CompleteOk (map CompletionMatch completions) (CursorRange start offset) mempty
