module Args
  ( Args(..)
  , parseArgs
  ) where

import Import

import Data.Monoid (Alt(..))
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (Doc, bold, vsep)

import qualified Data.List.NonEmpty as NonEmpty

data Args
  = Encode Word16 Word16 (Maybe Text)
  | Decode (NonEmpty Text)
  | Encrypt Word16 Word16 (Maybe Text)
  | Decrypt Text (NonEmpty Text)
  deriving Show

parseArgs :: IO Args
parseArgs = customExecParser (prefs prefsMod) parserInfo
 where
  prefsMod :: PrefsMod
  prefsMod = mconcat
    [ multiSuffix "..."
    , showHelpOnError
    , showHelpOnEmpty
    ]

  parserInfo :: ParserInfo Args
  parserInfo =
    info
      (helper <*> parser)
      (mconcat
        [ header "ssss - Shamir's Secret Sharing"
        ])

  parser :: Parser Args
  parser =
    getAlt (foldMap (Alt . hsubparser)
      [ encodeCommand
      , decodeCommand
      , encryptCommand
      , decryptCommand
      ])

encodeCommand :: Mod CommandFields Args
encodeCommand = mconcat
  [ command "encode"
      (info parser (mconcat
        [ headerDoc (Just hdr)
        , progDesc "Encode a short secret"
        , footerDoc (Just foot)
        ]))
  , metavar "encode"
  , shortCommandGroup
  ]
 where
  hdr :: Doc
  hdr = vsep
    [ "ssss encode - Shamir's Secret Sharing"
    , ""
    , "Encode a secret as " <> bold "N" <> " shares such that only " <> bold "K" <> " are required to decode it."
    , ""
    , "Share length is proportional to secret length, so prefer " <> bold "ssss encrypt" <> " for"
    , "secrets longer than 256 bits."
    , ""
    , "If a secret is not provided, it will be read from standard input."
    ]

  foot :: Doc
  foot = vsep
    [ "Examples:"
    , "  ssss encode 3 5 'Hello, world!'"
    , "  ssss encode 3 5 secret.txt"
    , "  cat secret.txt | ssss encode 3 5"
    ]

  parser :: Parser Args
  parser = Encode
    <$> threshold
    <*> shares
    <*> optional (strArgument (mconcat
          [ help "Secret to encode (filename or string)"
          , metavar "SECRET"
          ]))

decodeCommand :: Mod CommandFields Args
decodeCommand = mconcat
  [ command "decode"
      (info parser (mconcat
        [ headerDoc (Just hdr)
        , progDesc "Decode a short secret"
        , footerDoc (Just foot)
        ]))
  , metavar "decode"
  , shortCommandGroup
  ]
 where
  hdr :: Doc
  hdr = vsep
    [ "ssss decode - Shamir's Secret Sharing"
    , ""
    , "Decode a secret with " <> bold "K" <> " shares."
    ]

  foot :: Doc
  foot = vsep
    [ "Examples:"
    , "  ssss decode «share1» «share2» «share3»"
    , "  ssss decode share1.txt share2.txt share3.txt"
    ]

  parser :: Parser Args
  parser = Decode . NonEmpty.fromList
    <$> some (strArgument (mconcat
          [ help "Shares to decode (filenames or strings)"
          , metavar "SHARE"
          ]))

encryptCommand :: Mod CommandFields Args
encryptCommand = mconcat
  [ command "encrypt"
      (info parser (mconcat
        [ headerDoc (Just hdr)
        , progDesc "Encrypt a secret"
        , footerDoc (Just foot)
        ]))
  , metavar "encrypt"
  , longCommandGroup
  ]
 where
  hdr :: Doc
  hdr = vsep
    [ "ssss encrypt - Shamir's Secret Sharing"
    , ""
    , "Encrypt a secret using the Salsa20 stream cipher, then encode the encryption key"
    , "as " <> bold "N" <> " shares such that only " <> bold "K" <> " are required to decode it."
    , ""
    , "If a secret is not provided, it will be read from standard input."
    , ""
    , "The key shares are written to standard output, and the secret ciphertext is"
    , "written to standard error."
    ]

  foot :: Doc
  foot = vsep
    [ "Examples:"
    , "  ssss encrypt 3 5 plain.txt 2>cipher.txt"
    , "  cat plain.txt | ssss encrypt 3 5 2>cipher.txt"
    ]

  parser :: Parser Args
  parser = Encrypt
    <$> threshold
    <*> shares
    <*> optional (strArgument (mconcat
          [ help "Secret to encrypt (filename or string)"
          , metavar "SECRET"
          ]))

decryptCommand :: Mod CommandFields Args
decryptCommand = mconcat
  [ command "decrypt"
      (info parser (mconcat
        [ headerDoc (Just hdr)
        , progDesc "Decrypt a secret"
        , footerDoc (Just foot)
        ]))
  , metavar "decrypt"
  , longCommandGroup
  ]
 where
  hdr :: Doc
  hdr = vsep
    [ "ssss encrypt - Shamir's Secret Sharing"
    , ""
    , "Decrypt a secret with " <> bold "K" <> " shares."
    ]

  foot :: Doc
  foot = vsep
    [ "Examples:"
    , "  ssss decrypt cipher.txt «share1» «share2» «share3»"
    , "  ssss decrypt cipher.txt share1.txt share2.txt share3.txt"
    , "  cat cipher.txt | ssss decrypt - «share1» «share2» «share3»"
    , "  cat cipher.txt | ssss decrypt - share1.txt share2.txt share3.txt"
    ]

  parser :: Parser Args
  parser = Decrypt
    <$> strArgument (mconcat
          [ help "Secret file to decrypt ('-' means stdin)"
          , metavar "SECRET"
          ])
    <*> map NonEmpty.fromList
          (some (strArgument (mconcat
            [ help "Encryption key shares (filenames or strings)"
            , metavar "SHARE"
            ])))

threshold :: Parser Word16
threshold =
  argument auto
    (mconcat
      [ help "Number of shares required to recover the secret"
      , metavar "K"
      ])

shares :: Parser Word16
shares =
  argument auto
    (mconcat
      [ help "Total number of shares"
      , metavar "N"
      ])

shortCommandGroup :: Mod CommandFields Args
shortCommandGroup = commandGroup "Short secret commands (≤ 256 bits)"

longCommandGroup :: Mod CommandFields Args
longCommandGroup = commandGroup "Long secret commands (> 256 bits)"
