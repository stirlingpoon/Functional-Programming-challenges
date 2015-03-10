{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Challenge1 where

import System.Random
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import Data.Char
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy.Char8 as BSL
import Debug.Trace
import Data.List (minimumBy, delete, transpose)
import Data.Ord (comparing)
import Data.Either

import Text.Regex.PCRE ((=~))
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import Data.ByteString.Base64
import Data.Tuple.Select
import "language-c" Language.C
import "language-c" Language.C.System.GCC


type CSource = BS.ByteString

{-# NOINLINE golfIt #-}
golfIt :: CSource -> CSource
golfIt sourceCode = safeCleanUp $ unsafePerformIO $ preprocess sourceCode
{-
golfIt sourceCode = if BS.length newCode < BS.length sourceCode then newCode else sourceCode
	where
		newCode = unsafePerformIO $ safePreprocess sourceCode
-}

preprocess :: CSource -> IO CSource
preprocess sourceCode =
	do
		-- Write source file to temp file
		g <- newStdGen
		let (r, g1) = random g :: (Int, StdGen)
		let filename = "tmp" ++ show r ++ ".c"
		BS.writeFile filename sourceCode

		-- Generate AST
		result <- parseCFile' filename
		case result of
			Left err -> do
				-- Error encountered, just return the original source code
				removeFile filename
				return sourceCode
			Right ast -> do
				removeFile filename
				-- Recover source code and clean up
				return (if BS.length newSource <= BS.length sourceCode then newSource else sourceCode)
					where newSource = unsafeCleanUp $ BS.pack $ show $ prettyUsingInclude ast

parseCFile' = parseCFile (newGCC "/usr/bin/gcc") Nothing exts
	where exts = ["-std=c99", "-U__BLOCKS__" {- Remove Mac OS X blocks extension -}]

{-
convertAst :: CTranslUnit -> CTranslUnit
convertAst (CTranslUnit tUnit nodeInfo) =
	CTranslUnit tUnit nodeInfo
-}

unsafeCleanUp :: CSource -> CSource
unsafeCleanUp sourceCode = removeSpaces $ BS.concat $ removeIncludes . removeComments . removeIntents $ BS.lines sourceCode

safeCleanUp :: CSource -> CSource
safeCleanUp = replaceKeywords

removeSpaces :: CSource -> CSource
removeSpaces sourceCode = combineStrings (map removeSpace lines) strings
	where
		(lines, strings) = separateStrings sourceCode

combineStrings :: [CSource] -> [CSource] -> CSource
combineStrings lines strings = BS.concat $ concat $ transpose [lines, strings]

separateStrings sourceCode = ((regexSplit stringPattern sourceCode), (regexMatches stringPattern sourceCode))
	where
		stringPattern = "\"([^\"\\\\]*(\\\\.[^\"\\\\]*)*)\"|\\'([^\\'\\\\]*(\\\\.[^\\'\\\\]*)*)\\'"

removeSpace :: CSource -> CSource
removeSpace s = regexSub "(?<!\\w)\\s|\\s(?!\\w)" s ""


removeIntents :: [CSource] -> [CSource]
removeIntents = map (BS.dropWhile isSpace)

{-
	Assume
		- Each commment is on it's own line
		- Is of the form //... or /* ... */
-}
removeComments :: [CSource] -> [CSource]
removeComments = filter (\x -> not (isMultilineComment x || isDoubleSlashComment x))
	where
		isMultilineComment l = BS.isPrefixOf "/*" l && BS.isSuffixOf "*/" l
		isDoubleSlashComment = BS.isPrefixOf "//"

-- Assume each include is on it's own line and starts with #include
removeIncludes :: [CSource] -> [CSource]
removeIncludes = filter (not . isInclude)
	where
		isInclude = BS.isPrefixOf "#include"

alphanum = ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['a' .. 'z']
letters = ['A' .. 'Z'] ++ ['a' .. 'z']
validReplacements :: CSource -> [CSource]
validReplacements sourceCode =
	filter (not . inSourceCode) $ map BS.singleton letters ++ [BS.pack [c1, c2] | c1 <- letters, c2 <- alphanum]
	where
		inSourceCode s = BS.isInfixOf s sourceCode

makeDefineDirective :: CSource -> CSource -> CSource
makeDefineDirective keyword replacement =
	BS.concat ["#define ", replacement, " ", keyword, "\n"]

suffixes = map BS.pack ["{", "}", "(", ")", ";", "*", "**", "};", ");", "){", "*)", "**)"]
prefixes = map BS.pack ["{", "}", "(", ")", "(\""]
keywords = map BS.pack
	[ "const"
	, "return"
	, "for"
	, "while"
	, "switch"
	, "case"
	, "else"
	, "printf"
	, "scanf"
	, "int"
	, "void"
	, "char"
	, "long"
	, "long long"
	, "double"
	, "float"
	, "unsigned"
	, "null"
	, "struct"
	, "typedef"
	, "typedef struct"
	, "malloc"
	, "sizeof"
	, "malloc(sizeof"
	]
allKeywords = keywords ++ [BS.append p k | p <- prefixes, k <- keywords] ++ [BS.append k s | k <- keywords, s <- suffixes] ++ [BS.concat [p,k,s] | p <- prefixes, k <- keywords, s <- suffixes]

metachars = [ "\\" , "|" , "(" , ")" , "[" , "{" , "^" , "$" , "*" , "+" , "?" , "." ]
regexEscape :: CSource -> CSource
regexEscape "" = ""
regexEscape xs = BS.append (if s `elem` metachars then BS.append "\\" s else s) (regexEscape $ BS.tail xs)
	where s = BS.singleton $ BS.head xs

regexSub :: CSource -> CSource -> CSource -> CSource
regexSub regex string replacement = head $ rights [substituteCompile' regex string replacement]

regexSplit :: CSource -> CSource -> [CSource]
regexSplit regex string = head $ rights [splitCompile' regex string]

regexMatches :: CSource -> CSource -> [CSource]
regexMatches pattern string = map head (string =~ pattern :: [[CSource]])

-- replaceKeyword keyword replacement sourceCode
replaceKeyword :: CSource -> CSource -> CSource -> CSource
replaceKeyword k r sourceCode =
	if newLength < originalLength then newSource else sourceCode
	where
		defineCode = makeDefineDirective k r
		regex = BS.concat ["(^|(?<!\\w))", regexEscape k, "($|(?!\\w))"]
		replacedCode = regexSub regex sourceCode r
		newSource = BS.append defineCode replacedCode
		newLength = BS.length newSource
		originalLength = BS.length sourceCode

replaceKeywordsOnce :: [CSource] -> [CSource] -> CSource -> ([CSource], [CSource], CSource)
replaceKeywordsOnce ks rs sourceCode =
	if length newSources > 0 then ([k | (s, k) <- newSources, k /= snd newSource], tail rs, fst newSource) else (ks, rs, sourceCode)
	where
		r = head rs
		originalLength = BS.length sourceCode
		newSources = filter (\(s, k) -> BS.length s < originalLength) $ map (\k -> (replaceKeyword k r sourceCode, k)) ks
		newSource = minimumBy (comparing (BS.length . fst)) newSources

replaceKeywordsMulti :: [CSource] -> [CSource] -> CSource -> CSource
replaceKeywordsMulti ks rs sourceCode =
	if nks == ks then nc else replaceKeywordsMulti nks nrs nc
	where
		(nks, nrs, nc) = replaceKeywordsOnce ks rs sourceCode

replaceKeywords :: CSource -> CSource
replaceKeywords sourceCode =
	replaceKeywordsMulti allKeywords replacements sourceCode
	where replacements = validReplacements sourceCode


{-
replaceKeywords :: CSource -> CSource
replaceKeywords sourceCode =
	replaceKeywordsWithMappings (makeKeywordMappings $ validReplacements sourceCode) sourceCode

replaceKeywordsWithMappings :: [KeywordMapping] -> CSource -> CSource
replaceKeywordsWithMappings [] sourceCode = sourceCode
replaceKeywordsWithMappings (k:ks) sourceCode = replaceKeywordsWithMappings ks (replaceKeyword k sourceCode)
-}

-- Load test cases
{-# NOINLINE unsafeLoad #-}
unsafeLoad :: Int -> CSource
unsafeLoad n = unsafePerformIO $ BS.readFile $ "test/" ++ show n ++ ".c"

printCSource :: CSource -> IO ()
printCSource s = putStr $ BS.unpack s