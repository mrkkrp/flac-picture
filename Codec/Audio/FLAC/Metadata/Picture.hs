-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Picture
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Juicy-Pixels-powered helpers to read\/write images to FLAC metadata
-- blocks. For maximal player support, use PNG or JPEG (we don't provide
-- helpers for other formats at the time anyway).

module Codec.Audio.FLAC.Metadata.Picture
  ( retrieveImage
  , writeJpegPicture
  , writePngPicture )
where

import Codec.Audio.FLAC.Metadata
import Codec.Picture

-- | Read specific picture from FLAC metadata as 'DynamicImage'.

retrieveImage
  :: PictureType
  -> FlacMeta (Maybe DynamicImage)
retrieveImage = undefined -- TODO

-- | Write the given 'DynamicImage' into FLAC metadata block corresponding
-- to specific 'PictureType'.

writeJpegPicture
  :: PictureType       -- ^ Type of picture we're writing
  -> DynamicImage      -- ^ The picture to write
  -> FlacMeta ()
writeJpegPicture = undefined -- TODO

-- | Write the given 'DynamicImage' into FLAC metadata block corresponding
-- to specific 'PictureType'.

writePngPicture
  :: PictureType       -- ^ Type of picture we're writing
  -> DynamicImage      -- ^ The picture to write
  -> FlacMeta ()
writePngPicture = undefined -- TODO
