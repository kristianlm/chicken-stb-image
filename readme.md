  [CHICKEN]: http://call-cc.org
  [stb_image.h]: https://github.com/nothings/stb

# chicken-stb-image

This is a [CHICKEN] egg that wraps [stb_image.h] version 2.19 from
Sean Barrett and friends.

# API

    [procedure] (read-image #!key channels)
    [procedure] (load-image blob #!key channels)

Decodes images. `read-image` reads the image from from
`(current-input-port)` while `load-image` gets its data through a blob
or a string. The image type is detected by content. Returns 4 values:

1. raw pixel data (as blob)
2. width
3. height
4. number of channels

You can force the number of channels with the `channels` keyword. If
`channels` is `#f` or not given, the number of channels in the
original image will be used.

The size of the pixel data is always `(* width height channels)`
bytes. The first pixel is the top-left-most in the image. Each pixel
is `channel` number of bytes long. The number of channels define the
pixel format interleaved in order as follows:

1. grey
2. grey, alpha
3. red, green, blue
4. red, green, blue, alpha

The supported image types are: `png`, `bmp`, `tga` and
`jpg`. [stb_image.h]'s `hdr` support only works with `load-image`.

    [procedure] (read-image-info)
	[procedure] (load-image-info blob)

Like `read-image` and `load-image`, but does not load pixel data and
should be faster. Returns three values:

1. width
2. height
3. number of channels

## Examples


    (receive
         (with-input-from-string
           (conc "\211PNG\r\n\x1a\n\x00\x00\x00\r"
               "IHDR\x00\x00\x00\x02\x00\x00\x00\x02\b\x00"
               "\x00\x00\x00W\335R\370\x00\x00\x00\x11"
               "IDAT\b\x1d\x01\x06\x00\371\377\x00\x00E\x00"
               "\253\377\x03o\x01\xf0\xd7\xb5\x30}\x00\x00\x00\x00"
               "IEND\256B`\202")
           read-image))

Returns `'(#${00 45 ab ff} 2 2 1)` for the provided 2x2 grayscale PNG
image.
