-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Picture
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Juicy-Pixels-powered helpers to read\/write images to FLAC metadata
-- blocks. For maximal player support, use PNG or JPEG (we don't provide
-- helpers for other formats at the time anyway).

{-# LANGUAGE OverloadedStrings #-}

module Codec.Audio.FLAC.Metadata.Picture
  ( retrieveImage
  , writeJpegPicture
  , writePngPicture )
where

import Codec.Audio.FLAC.Metadata
import Codec.Picture
import Data.Word
import qualified Data.ByteString.Lazy as BL

-- | Read specific picture from FLAC metadata as 'DynamicImage'.

retrieveImage
  :: PictureType
  -> FlacMeta (Either String DynamicImage)
retrieveImage pictureType = do
  mpicture <- retrieve (Picture pictureType)
  case mpicture of
    Nothing -> return (Left "Picture not found")
    Just picture -> (return . decodeImage . pictureData) picture

-- | Write the given image into FLAC metadata block corresponding to
-- specific 'PictureType'.

writeJpegPicture
  :: PictureType       -- ^ Type of picture we're writing
  -> Word8             -- ^ Quality factor, see 'encodeJpegAtQuality'
  -> Image PixelYCbCr8 -- ^ The picture to write
  -> FlacMeta ()
writeJpegPicture pictureType q image =
  Picture pictureType =-> Just PictureData
    { pictureMimeType    = "image/jpeg"
    , pictureDescription = ""
    , pictureWidth       = fromIntegral (imageWidth image)
    , pictureHeight      = fromIntegral (imageHeight image)
    , pictureDepth       = 24
    , pictureColors      = 0 -- non-indexed
    , pictureData        = BL.toStrict (encodeJpegAtQuality q image)
    }

-- | Write the given image into FLAC metadata block corresponding to
-- specific 'PictureType'.

writePngPicture
  :: PictureType       -- ^ Type of picture we're writing
  -> Image PixelRGB8   -- ^ The picture to write
  -> FlacMeta ()
writePngPicture pictureType image =
  Picture pictureType =-> Just PictureData
    { pictureMimeType    = "image/png"
    , pictureDescription = ""
    , pictureWidth       = fromIntegral (imageWidth image)
    , pictureHeight      = fromIntegral (imageHeight image)
    , pictureDepth       = 24
    , pictureColors      = 0 -- non-indexed
    , pictureData        = BL.toStrict (encodePng image)
    }
