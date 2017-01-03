--
-- Tests for the ‘flac-picture’ package.
--
-- Copyright © 2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Codec.Audio.FLAC.Metadata.PictureSpec
  ( spec )
where

import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.Metadata.Picture
import Codec.Picture
import Control.Monad
import Data.Default.Class
import Data.Function (on)
import System.Directory
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = around withSandbox $ do
  describe "writeJpegPicture/retrieveImage" . forM_ [minBound..maxBound] $ \ptype ->
    it ("writes JPEG picture and reads it back: " ++ show ptype) $ \path -> do
      Right (ImageYCbCr8 pic) <- readJpeg "picture-samples/lenna.jpeg"
      runFlacMeta def path (writeJpegPicture ptype 127 pic)
      let Right (ImageYCbCr8 pic'') = (decodeJpeg . BL.toStrict)
            (encodeJpegAtQuality 100 pic)
      Just PictureData {..} <- runFlacMeta def path (retrieve $ Picture ptype)
      pictureMimeType `shouldBe` "image/jpeg"
      pictureDescription `shouldBe` ""
      pictureWidth `shouldBe` fromIntegral (imageWidth pic)
      pictureHeight `shouldBe` fromIntegral (imageHeight pic)
      pictureDepth `shouldBe` 24
      pictureColors `shouldBe` 0
      Right (ImageYCbCr8 pic') <- runFlacMeta def path (retrieveImage ptype)
      pic' `imageShouldBe` pic''
  describe "writePngPicture/retrieveImage" . forM_ [minBound..maxBound] $ \ptype ->
    it ("writes PNG picture and reads it back: " ++ show ptype) $ \path -> do
      Right (ImageRGB8 pic) <- readPng "picture-samples/lenna.png"
      runFlacMeta def path (writePngPicture ptype pic)
      Just PictureData {..} <- runFlacMeta def path (retrieve $ Picture ptype)
      pictureMimeType `shouldBe` "image/png"
      pictureDescription `shouldBe` ""
      pictureWidth `shouldBe` fromIntegral (imageWidth pic)
      pictureHeight `shouldBe` fromIntegral (imageHeight pic)
      pictureDepth `shouldBe` 24
      pictureColors `shouldBe` 0
      Right (ImageRGB8 pic') <- runFlacMeta def path (retrieveImage ptype)
      pic' `imageShouldBe` pic

----------------------------------------------------------------------------
-- Helpers

-- | Make a temporary copy of @audio-samples/sample.flac@ file and provide
-- the path to the file. Automatically remove the file when the test
-- finishes.

withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempFile "sample.flac" $ \path h -> do
  hClose h
  copyFile "audio-samples/sample.flac" path
  action path

imageShouldBe
  :: (Pixel a, Show (PixelBaseComponent a), Eq (PixelBaseComponent a))
  => Image a
  -> Image a
  -> Expectation
imageShouldBe img0 img1 = do
  (shouldBe `on` imageWidth)  img0 img1
  (shouldBe `on` imageHeight) img0 img1
  (shouldBe `on` imageData)   img0 img1
