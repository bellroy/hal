{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : AWS.Lambda.Events.SQS
Description : Data types for working with SQS events.
Copyright   : (c) Nike, Inc., 2019
License     : BSD3
Maintainer  : nathan.fairhurst@nike.com, fernando.freire@nike.com
Stability   : stable
-}

module AWS.Lambda.Events.SQS (
  Records (..),
  Attributes (..),
  MessageAttributeValue (..),
  SQSEvent (..)
) where

import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import           GHC.Generics           (Generic)

-- | Represents an event from AWS SQS.
--
-- See the <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html AWS documentation>
-- for a sample payload.
newtype Records = Records {
  records :: [SQSEvent]
} deriving (Show, Eq, Generic)

instance FromJSON Records where
  parseJSON = withObject "Records" $ \v -> Records <$> v .: "Records"

data Attributes = Attributes {
  approximateReceiveCount          :: Text,
  sentTimestamp                    :: Text,
  senderId                         :: Text,
  approximateFirstReceiveTimestamp :: Text,
  messageGroupId                   :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \v -> do
    approximateReceiveCount <- v .: "ApproximateReceiveCount"
    sentTimestamp <- v .: "SentTimestamp"
    senderId <- v .: "SenderId"
    approximateFirstReceiveTimestamp <- v .: "ApproximateFirstReceiveTimestamp"
    messageGroupId <- v .:? "MessageGroupId"
    pure Attributes {..}

data MessageAttributeValue = MessageAttributeValue {
  stringValue      :: Maybe Text,
  binaryValue      :: Maybe ByteString,
  stringListValues :: [Text],
  binaryListValues :: [ByteString],
  dataType         :: Text
} deriving (Show, Eq, Generic)

instance FromJSON MessageAttributeValue where
  parseJSON = withObject "MessageAttributeValue" $ \v -> do
    stringValue <- v .:? "stringValue"
    binaryValue <- fmap decodeBase64Text <$> v .:? "binaryValue"
    stringListValues <- maybe [] id <$> v .:? "stringListValues"
    binaryListValues <- maybe [] (map decodeBase64Text) <$> v .:? "binaryListValues"
    dataType <- v .: "dataType"
    pure MessageAttributeValue {..}

data SQSEvent = SQSEvent {
  messageId         :: Text,
  receiptHandle     :: Text,
  body              :: Text,
  attributes        :: Attributes,
  messageAttributes :: Map Text MessageAttributeValue,
  md5OfBody         :: Text,
  eventSource       :: Text,
  eventSourceARN    :: Text,
  awsRegion         :: Text
} deriving (Show, Eq, Generic)

instance FromJSON SQSEvent

decodeBase64Text :: Text -> ByteString
decodeBase64Text = B64.decodeLenient . TE.encodeUtf8
