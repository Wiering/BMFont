
' (C) Copyright 2008, Mike Wiering, Wiering Software
' 
' Permission is hereby granted, free of charge, to any person obtaining a copy
' of this software and associated documentation files (the "Software"), to deal
' in the Software without restriction, including without limitation the rights
' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
' copies of the Software, and to permit persons to whom the Software is
' furnished to do so, subject to the following conditions:
' 
' The above copyright notice and this permission notice shall be included in
' all copies or substantial portions of the Software.
' 
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
' THE SOFTWARE.

SuperStrict

Rem
bbdoc: bmfont
End Rem
Module Wiering.BMFont

ModuleInfo "Name: BMFont"
ModuleInfo "Description: Library for AngelCode Bitmap Font Generator (www.angelcode.com/products/bmfont)"
ModuleInfo "Version: 1.0"
ModuleInfo "License: MIT"
ModuleInfo "Author: Mike Wiering"
ModuleInfo "Copyright: (C) 2008, Mike Wiering, Wiering Software"

ModuleInfo "History: 1.0"
ModuleInfo "History: Initial Release."


Import BRL.D3D7Max2D
Import BRL.GLMax2D
Import BRL.Stream
Import BRL.Max2D
'Import BRL.PNGLoader
'Import BRL.BMPLoader
'Import BRL.TGALoader
'Import BRL.JPGLoader


Const ALIGN_LEFT: Int    = -1
Const ALIGN_CENTER: Int  =  0
Const ALIGN_RIGHT: Int   =  1

Rem
bbdoc: Type TBMFont
about: Bitmap Font Object
End Rem
Type TBMFont

  Field BMFImage: TImage[]
  Field CustomImage: TImage[]
  Field ShadowImage: TImage[]
  Field DrawingShadow: Int
  Field Image: TImage[]
  Field ImageW: Int[]
  Field ImageH: Int[]

  Rem
  bbdoc: Sets whether the shadow should be drawn (use CreateShadow to create the shadow first).
  about: This can be used for italic (Slant = 0.25).
  End Rem
  Field Shadow: Int

  Rem
  bbdoc: Horizontal shadow offset.
  about: Relative to the text.
  End Rem
  Field ShadowX: Int

  Rem
  bbdoc: Vertical shadow offset.
  about: Relative to the text.
  End Rem
  Field ShadowY: Int

  Rem
  bbdoc: Horizontal spacing in pixels.
  about: This can be used to add extra spacing between characters.
  End Rem
  Field HSpacing: Float

  Rem
  bbdoc: Vertical spacing in pixels.
  about: This can be used to add extra spacing between lines.
  End Rem
  Field VSpacing: Float

  Rem
  bbdoc: Sets whether kerning should be used or not (on by default if FNT file has kerning).
  about: Kerning adjusts the distance between certain pairs of characters.
  End Rem
  Field Kerning: Int

  Rem
  bbdoc: Sets the horizontal text alignment.
  about: This can be ALIGN_LEFT (default), ALIGN_CENTER or ALIGN_RIGHT.
  End Rem
  Field Align: Int

  Rem
  bbdoc: Draws the text at a slant position (DirectX only!).
  about: This can be used for italic (Slant = 0.25).
  End Rem
  Field Slant: Float


  Field FontSize: Short
  Field Bold: Byte
  Field Italic: Byte
  Field Unicode: Byte
  Field Smooth: Byte
  Field CharSet: Byte
  Field StretchH: Short
  Field AA: Byte
  Field PaddingUp: Byte
  Field PaddingRight: Byte
  Field SpacingHoriz: Byte
  Field SpacingVert: Byte
  Field Outline: Byte

  Field LineHeight: Int
  Field Base: Int
  Field ScaleW: Int
  Field ScaleH: Int
  Field Pages: Int
  Field Packed: Byte
  Field Encoded: Byte

  Field IdStr: String
  Field Id: Short[]
  Field X: Short[]
  Field Y: Short[]
  Field Width: Short[]
  Field Height: Short[]
  Field XOffset: Short[]
  Field YOffset: Short[]
  Field XAdvance: Short[]
  Field Page: Byte[]

  Field ASCII: Short[]

  Field KerningChars: String[]
  Field KerningValue: String[] 

  Field XScale: Float = 1
  Field YScale: Float = 1

  Field LastText: String = ""
  Field LastWidth: Float = 0
  Field LastXScale: Float = 1
  Field LastYScale: Float = 1
  Field LastLine: Float = -1

  Field dx: Float = 0
  Field dy: Float = 0

  Rem
  bbdoc: String of characters that generate a callback.
  about: This can be used for control characters that change the color or offset (dx, dy).
  End Rem
  Field CallBackChars: String

  Rem
  bbdoc: String of characters that should not be drawn.
  about: This can be used for control characters.
  End Rem
  Field SkipChars: String


  Rem
  bbdoc: Callback function that is called before drawing a character that occurs in the string CallBackChars.
  about: Parameters:
  <ul>
  <li><b>Char</b>: The character that is about to be drawn</li>
  <li><b>X</b>: The X position</li>
  <li><b>Y</b>: The Y position</li>
  </ul>
  End Rem
  Method OnCallBack (Char: Int, X: Float, Y: Float)
    ' overload this
  End Method


  Rem
  bbdoc: Creates a new TBMFont object from a binary .fnt file.
  about: Parameters:
  <ul>
  <li><b>Prefix</b>: This can be either "incbin::" or a path like "fonts\"</li>
  <li><b>FNTFile</b>: This must be a binary FNT file (version 2)</li>
  </ul>
  End Rem
  Function Create: TBMFont (Prefix: String, FNTFile: String)
    Local this: TBMFont = New TBMFont
    this.Init (Prefix, FNTFile)
    Return this
  End Function
  

  Rem
  bbdoc: Initializes a new TBMFont object from a binary .fnt file.
  about: Parameters:
  <ul>
  <li><b>Prefix</b>: This can be either <b>incbin::</b> or a path like <b>CurrentDir() + "/"</b></li>
  <li><b>FNTFile</b>: This must be a binary FNT file (version 2)</li>
  </ul>
  End Rem
  Method Init (Prefix: String, FNTFile: String)
    Local file: TStream = ReadStream ("littleendian::" + Prefix + FNTFile)
    If ReadString (file, 4) <> "BMF" + Chr(2) Then
?debug
      DebugLog Prefix + FNTFile + " is not a binary version 2 .FNT file"
?
    Else
      Local BlockType: Int = ReadByte (file)
      While BlockType <> 0
        Local BlockLen: Int = ReadInt (file) - 4

        Select BlockType
          Case 1  ' info not used here
            LoadInfo (file, BlockLen)
          Case 2  ' common
            LineHeight = ReadShort (file)
            Base = ReadShort (file)
            ScaleW = ReadShort (file)
            ScaleH = ReadShort (file)
            Pages = ReadShort (file)
            Local b: Byte = ReadByte (file)
            Packed = (b & 1) <> 0
            Encoded = (b & 2) <> 0
          Case 3  ' pages
            LoadPages (Prefix, ReadString (file, BlockLen))
          Case 4  ' characters
            LoadCharacters (file, BlockLen / 18)
          Case 5  ' kerning
            LoadKerning (file, BlockLen / 6)
          Default
?Debug
            DebugLog "Unknown block: " + BlockType + ", length: " + BlockLen
?
            ReadString (file, BlockLen)  ' skip unknown block

        End Select

        BlockType = ReadByte (file)
      Wend
    End If
    CloseStream file
    HSpacing = 0
    VSpacing = 0
    Align = ALIGN_LEFT
    Shadow = False
    DrawingShadow = False
    Slant = 0.0
  End Method


  Method LoadInfo (file: TStream, BlockLen: Int)
    FontSize = ReadShort (file)    
    Local b: Byte = ReadByte (file)
    Bold = (b & $80) <> 0
    Italic = (b & $40) <> 0
    Unicode = (b & $20) <> 0
    Smooth = (b & $10) <> 0
    CharSet = ReadByte (file)
    StretchH = ReadShort (file)
    AA = ReadByte (file)
    PaddingUp = ReadByte (file)
    PaddingRight = ReadByte (file)
    SpacingHoriz = ReadByte (file)
    SpacingVert = ReadByte (file)
    Outline = ReadByte (file)
    ReadString (file, BlockLen - 12)  ' skip name
  End Method


  Function Pow2Size: Int (n: Int)
    Local ry: Int = 1
    While ry < n
      ry = ry Shl 1
    Wend
    Return ry
  End Function


  Method LoadPages (Prefix: String, ImageFile: String)
    Local i: Int
    BMFImage = BMFImage[..Pages]
    For i = 0 To Pages - 1
      BMFImage[i] = LoadImage (Prefix + ImageFile.Split (Chr (0))[i])
    Next
    If Packed Then
      DecodeChannels ()
    End If
    ImageW = ImageW[..Pages]
    ImageH = ImageH[..Pages]
    For i = 0 To Pages - 1
      ImageW[i] = Pow2Size (BMFImage[i].Width)
      ImageH[i] = Pow2Size (BMFImage[i].Height)
    Next
    Image = BMFImage
    CustomImage = BMFImage
    ShadowImage = BMFImage
    EnableCustomTexture (False)
  End Method


  Method LoadCharacters (file: TStream, N: Int)
    Local i: Int
    ASCII = ASCII[..256]
    IdStr = ""
    Id = Id[..N]
    X = X[..N]
    Y = Y[..N]
    Width = Width[..N]
    Height = Height[..N]
    XOffset = XOffset[..N]
    YOffset = YOffset[..N]
    XAdvance = XAdvance[..N]
    Page = Page[..N]
    For i = 0 To N - 1
      Id[i] = ReadShort (file)
      If Id[i] < 256 Then
        ASCII[Id[i]] = i
      End If
      IdStr :+ Chr (Id[i])
      X[i] = ReadShort (file)
      Y[i] = ReadShort (file)
      Width[i] = ReadShort (file)
      Height[i] = ReadShort (file)
      XOffset[i] = ReadShort (file)
      YOffset[i] = ReadShort (file)
      XAdvance[i] = ReadShort (file)
      Page[i] = ReadByte (file)
      Local Channel: Int = ReadByte (file)
      If Packed Then
        Local ch: Int = 0
        Select Channel
          Case 2 ch = 1
          Case 4 ch = 2
          Case 8 ch = 3
        End Select
        Page[i] = 4 * Page[i] + ch
      End If
    Next
    Kerning = False
    KerningChars = KerningChars[..N]
    KerningValue = KerningValue[..N]
  End Method


  Function SignedShort: Int (X: Short)
    Return (X Shl 16) Sar 16
  End Function

  Function SignedByte: Int (X: Byte)
    Return (X Shl 24) Sar 24
  End Function


  Method FindChar: Int (Char: Short)
    If Char <= 255 Then
      Return ASCII[Char]
    Else
      Return IdStr.Find (Chr (Char))
    End If
  End Method


  Method LoadKerning (file: TStream, N: Int)
    For Local i: Int = 0 To N - 1
      Local First: Int = ReadShort (file)
      Local Second: Int = ReadShort (file)
      Local Amount: Int = ReadShort (file)
      Local j: Int = IdStr.Find (Chr (First))
      If j >= 0 Then
        KerningChars[j] :+ Chr (Second)
        KerningValue[j] :+ Chr (Amount & $FF)
        Kerning = True
      End If
    Next
  End Method


  Rem
  bbdoc: Returns the height of a string in pixels using the current scale.
  about: Parameters:
  <ul>
  <li><b>Text</b>: The string (can have multiple lines)</li>
  </ul>
  End Rem
  Method TextHeight: Float (Text: String)
    Local H: Int = 1
    GetScale (XScale, YScale)
    For Local i: Int = 0 To Text.Length - 1
      If Text[i] = Asc ("~n") Then
        H :+ 1
      End If
    Next
    Return H * YScale * (FontSize + SpacingVert + VSpacing)
  End Method


  Rem
  bbdoc: Returns the width of a string in pixels using the current scale.
  about: Parameters:
  <ul>
  <li><b>Text</b>: The string (can have multiple lines)</li>
  <li><b>Line=-1</b>: If supplied, TextWidth only counts this line</li>
  </ul>
  End Rem
  Method TextWidth: Float (Text: String, Line: Int = -1)
    Local M: Float = 0
    Local L: Int = 0

    Local Char: Int
    Local Index: Int
    Local LastIndex: Int = -1
    Local fdx: Float = 0

    GetScale (XScale, YScale)
    If (Text = LastText) And (XScale = LastXScale) And (YScale = LastYScale) And (Line = LastLine) Then
      Return LastWidth 
    End If
    LastText = Text
    LastXScale = XScale
    LastYScale = YScale
    LastLine = Line
    
    For Local i: Int = 0 To Text.Length - 1
      Char = Text[i]
      If Char = Asc ("~n") Then
        M = Max (M, fdx)
        fdx = 0
        L :+ 1
        LastIndex = -1
      Else
        If (Line = -1) Or (L = Line) Then 

          Index = FindChar (Char)

          If (Index >= 0) And (SkipChars.Find (Chr (Char)) = -1) Then

            If Kerning Then
              If LastIndex <> -1 Then
                Local k: Int = KerningChars[LastIndex].Find (Chr (Char))
                If k >= 0 Then
                  fdx :+ SignedByte (KerningValue[LastIndex][k] & $FF)
                End If
              End If
            End If
            fdx :+ SignedShort (XAdvance[Index])
            If i < Text.Length - 1 Then 
              fdx :+ SpacingHoriz + HSpacing
            End If
            LastIndex = Index

          End If
        End If
      End If
    Next
    M = XScale * Max (M, fdx)
    LastWidth = M
    Return M
  End Method


  Rem
  bbdoc: Draws the text using the bitmap font characters using the current values for Align, Shadow, ShadowX, ShadowY, HSpacing, VSpacing and Kerning. 
  about: Parameters:
  <ul>
  <li><b>Text</b>: The string (can have multiple lines)</li>
  <li><b>XPos</b>: Tye X position (center position for ALIGN_CENTER)</li>
  <li><b>YPos</b>: The Y position</li>
  </ul>
  End Rem
  Method DrawText (Text: String, XPos: Float, YPos: Float)

    If Shadow And (Not DrawingShadow) Then
      DrawingShadow = True
      Local tmpImage: TImage[] = Image
      Image = ShadowImage
      Local Blend: Int = GetBlend ()
      Local Alpha: Float = GetAlpha ()
      SetBlend SHADEBLEND
      Local ColorR: Int, ColorG: Int, ColorB: Int
      GetColor (ColorR, ColorG, ColorB)
      DrawText (Text, XPos + ShadowX, YPos + ShadowY)  ' draw shadow
      SetColor (ColorR, ColorG, ColorB)
      SetBlend Blend
      SetAlpha Alpha
      Image = tmpImage
      DrawingShadow = False
    End If

    Local Char: Int
    Local LastChar: Int = -1
    Local Index: Int
    Local LastIndex: Int = -1
    Local fdx: Float = 0
    Local fdy: Float = 0

    Local Line: Int = 0
    Local M: Float = 0
    Local W: Float = TextWidth (Text, Line) 
    'GetScale (XScale, YScale)  ' already done by TextWidth

    dx = 0  ' can be used in callback functions
    dy = 0

    If Align = ALIGN_RIGHT Then fdx :- W / XScale
    If Align = ALIGN_CENTER Then fdx :- (W / XScale) / 2
    For Local i: Int = 0 To Text.Length - 1
      Char = Text[i]

      Local CB: Int = CallBackChars.Find (Chr (Char)) > -1
      If CB Then
        OnCallBack (Char, XPos + XScale * fdx, YPos + YScale * fdy)
      End If

      If SkipChars.Find (Chr (Char)) = -1 Then

        If Char = Asc ("~n") Then
          M = Max (M, fdx)
          fdy :+ FontSize + SpacingVert + VSpacing
          Line :+ 1
          LastChar = -1
          LastIndex = -1
          W = TextWidth (Text, Line)
          fdx = 0
          If Align = ALIGN_RIGHT Then fdx :- W / XScale
          If Align = ALIGN_CENTER Then fdx :- (W / XScale) / 2
        Else
          Index = FindChar (Char)
          If Index >= 0 Then

            If Kerning Then
              If LastIndex <> -1 Then
                Local k: Int = KerningChars[LastIndex].Find (Chr (Char))
                If k >= 0 Then
                  fdx :+ SignedByte (KerningValue[LastIndex][k] & $FF)
                End If
              End If
            End If

            Local tw: Float = ImageW[Page[Index]]
            Local th: Float = ImageH[Page[Index]]
            Local xp: Float = X[Index]
            Local yp: Float = Y[Index]
            Local w: Float = Width[Index]
            Local h: Float = Height[Index]

            If DrawingShadow Then
              xp :- (SpacingHoriz / 2)
              yp :- (PaddingUp / 2)
              w :+ (SpacingHoriz + PaddingRight) / 2
              h :+ (PaddingUp + SpacingVert) / 2
              SetColor ($FF, $FF, $FF)
            End If

?Win32
            Local DXFrame: TD3D7ImageFrame = TD3D7ImageFrame (Image[Page[Index]].frame(0))
            If DXFrame Then
              'DXFrame.SetUV (xp / tw, yp / th, (xp + w) / tw, (yp + h) / th)
              SetUVEx (DXFrame, xp / tw, yp / th, (xp + w) / tw, (yp + h) / th, Slant)
              DrawImageRect (Image[Page[Index]], XPos + XScale * (fdx + SignedShort (XOffset[Index]) + dx), ..
                                                 YPos + YScale * (fdy + SignedShort (YOffset[Index]) + dy), w, h)
              DXFrame.SetUV(0, 0, Image[Page[Index]].Width / tw, Image[Page[Index]].Height / th)
            Else
?
              Local GLFrame: TGLImageFrame = TGLImageFrame (Image[Page[Index]].frame(0))
              GLFrame.u0 = xp / tw
              GLFrame.v0 = yp / th
              GLFrame.u1 = (xp + w) / tw
              GLFrame.v1 = (yp + h) / th
              DrawImageRect (Image[Page[Index]], XPos + XScale * (fdx + SignedShort (XOffset[Index]) + dx), ..
                                                 YPos + YScale * (fdy + SignedShort (YOffset[Index]) + dy), w, h)
              GLFrame.u0 = 0
              GLFrame.v0 = 0
              GLFrame.u1 = Image[Page[Index]].Width / tw
              GLFrame.v1 = Image[Page[Index]].Height / th
?Win32
            EndIf
?

            fdx :+ SpacingHoriz + SignedShort (XAdvance[Index]) + HSpacing
            LastChar = Char
            LastIndex = Index
          End If
        End If

      End If

    Next
  End Method


?win32
  Function SetUVEx (DXFrame: TD3D7ImageFrame, u0: Float, v0: Float, u1: Float, v1: Float, Slant: Float)
    Local x: Float = (v1 - v0) / 2
    DXFrame.xyzuv[4] = u0 - Slant * x
    DXFrame.xyzuv[5] = v0
    DXFrame.xyzuv[10] = u1 - Slant * x
    DXFrame.xyzuv[11] = v0
    DXFrame.xyzuv[16] = u1 + Slant * x
    DXFrame.xyzuv[17] = v1
    DXFrame.xyzuv[22] = u0 + Slant * x
    DXFrame.xyzuv[23] = v1
  End Function
?


  Function ClipByte: Int (X: Int)
    If X < 0 Return 0 Else If X > 255 Return 255 Else Return X
  End Function


  Rem
  bbdoc: Adds a texture to the font.
  about: Parameters:
  <ul>
  <li><b>Tex</b>: The texture image or NULL to reset</li>
  </ul>
  End Rem
  Method ApplyTexture (Tex: TImage)
    If Not Tex Then
      Image = BMFImage  ' reset
      CustomImage = BMFImage
      EnableCustomTexture (False)
    Else
      Local tw: Int = Tex.Width
      Local th: Int = Tex.Height
      Local pmTex: TPixmap = LockImage (Tex)
      Local N: Int = BMFImage.length
      CustomImage = CustomImage[..N]

      For Local img: Int = 0 To N - 1
        Local w: Int = BMFImage[img].Width
        Local h: Int = BMFImage[img].Height

        Local pmSrc: TPixmap = LockImage (BMFImage[img])
        Local pmDst: TPixmap = CreatePixmap (w, h, PF_BGRA8888)
        ClearPixels (pmDst, 0)

        For Local j: Int = 0 To Id.length - 1
          If Page[j] = img Then

            Local xx: Int = (tw / 2) - (XAdvance[j]) / 2 + SignedShort (XOffset[j])
            Local yy: Int = (th / 2) - (FontSize / 2) + SignedShort (YOffset[j])
            For Local jj: Int = 0 To Height[j] - 1
              For Local ii: Int = 0 To Width[j] - 1
                Local xp: Int = xx + ii
                Local yp: Int = yy + jj
                If xp < 0 Then xp = 0 Else If xp > tw - 1 Then xp = tw - 1
                If yp < 0 Then yp = 0 Else If yp > th - 1 Then yp = th - 1
                Local rgba1: Int = ReadPixel (pmSrc, ii + X[j], jj + Y[j])
                Local rgba2: Int = ReadPixel (pmTex, xp, yp)

                Local r1: Int = (rgba1 Shr 16) & $FF
                Local r2: Int = (rgba2 Shr 16) & $FF
                Local g1: Int = (rgba1 Shr 8) & $FF
                Local g2: Int = (rgba2 Shr 8) & $FF
                Local b1: Int = rgba1 & $FF
                Local b2: Int = rgba2 & $FF

                Local rgba: Int = (rgba1 & $FF000000) + ..
                    (ClipByte ((r1 * r2) / $C0) Shl 16) + ..
                    (ClipByte ((g1 * g2) / $C0) Shl 8) + ..
                     ClipByte ((b1 * b2) / $C0)

                WritePixel (pmDst, ii + X[j], jj + Y[j], rgba)
              Next
            Next

          End If
        Next
        CustomImage[img] = LoadImage (pmDst)
        UnlockImage (BMFImage[img])

      Next
      UnlockImage (Tex)
      
      Image = CustomImage
      EnableCustomTexture (True)
    End If
  End Method


  Rem
  bbdoc: Switches custom textures on or off.
  about: Parameters:
  <ul>
  <li><b>Value</b>: True for custom texture, False for no texture.</li>
  </ul>
  End Rem
  Method EnableCustomTexture (Value: Int)
    If Value Then
      Image = CustomImage
    Else
      Image = BMFImage
    End If
  End Method


  Method DecodeChannels ()
    Local ChannelImage: TImage[]
    ChannelImage = ChannelImage[..Pages * 4]
    For Local i: Int = 0 To Pages - 1
      Local w: Int = BMFImage[i].width
      Local h: Int = BMFImage[i].height
      Local pmSrc: TPixmap = LockImage (BMFImage[i])
      For Local j: Int = 0 To 3
        Local pmDst: TPixmap = CreatePixmap (w, h, PF_A8)
        For Local y: Int = 0 To h - 1
          For Local x: Int = 0 To w - 1
            Local argb: Int = ReadPixel (pmSrc, x, y)
            argb = ((argb Shl (8 * (3 - j))) & $FF000000) | $FFFFFF
            WritePixel pmDst, x, y, argb
          Next
        Next
        ChannelImage[i * 4 + j] = LoadImage (pmDst)
      Next
      UnlockImage (BMFImage[i])
    Next
    Pages = 4 * Pages
    BMFImage = ChannelImage
  End Method


  Rem
  bbdoc: Creates a soft shadow of the bitmap font (position can be set with ShadowX and ShadowY). WARNING: CreateShadow uses the backbuffer!
  about: Parameters:
  <ul>
  <li><b>ShadowBlur = 3</b>: Blur size in pixels (1..9), be sure to add padding space on all sides!</li>
  <li><b>Amount = 255</b>: Shadow darkness 1..255 or 0 to reset</li>
  </ul>
  End Rem
  Method CreateShadow (ShadowBlur: Float = 3, Amount: Int = 255)
    If Amount = 0 Then
      ShadowImage = Image
    Else
      Local ColorR: Int, ColorG: Int, ColorB: Int
      Local ClsColorR: Int, ClsColorG: Int, ClsColorB: Int
      Local Alpha: Float = GetAlpha ()
      Local Blend: Int = GetBlend ()

      GetColor (ColorR, ColorG, ColorB)
      GetClsColor (ClsColorR, ClsColorG, ClsColorB)

      Local DW: Int = 256
      Local DH: Int = 256
      Local BlurX: Int = ShadowBlur
      Local BlurY: Int = ShadowBlur

      Local N: Int = BMFImage.length
      ShadowImage = ShadowImage[..N]

      For Local img: Int = 0 To N - 1
        Local w: Int = BMFImage[img].Width
        Local h: Int = BMFImage[img].Height

        SetClsColor 0, 0, 0
        SetColor Amount, Amount, Amount
        SetBlend AlphaBlend

        Local pmDst: TPixmap = CreatePixmap (w, h, PF_BGRA8888)
        ClearPixels (pmDst, 0)

        For Local by: Int = 0 To (h + DH - 1) / DH - 1
          For Local bx: Int = 0 To (w + DW - 1) / DW - 1

            Cls
            For Local j: Int = 0 To BlurY - 1
              For Local i: Int = 0 To BlurX - 1
                Local A1: Float = Sin (45 + 90 * Float (i) / BlurX)
                Local A2: Float = Sin (45 + 90 * Float (j) / BlurY)
                If A1 < 0 Then A1 = 0
                If A2 < 0 Then A2 = 0
                SetAlpha ((A1 + A2) / (BlurX * BlurY))
                DrawImage (BMFImage[img], -bx * DW + i - BlurX / 2, -by * DH + j - BlurY / 2)
              Next
            Next

            Local pm: TPixmap = GrabPixmap (0, 0, DW, DH)
            Local TmpDH: Int = DH
            If by * DH + DH - 1 >= h Then TmpDH = h - by * DH
            Local TmpDW: Int = DW
            If bx * DW + DW - 1 >= w Then TmpDW = w - bx * DW
            For Local jj: Int = 0 To TmpDH - 1
              For Local ii: Int = 0 To TmpDW - 1
                Local rgba: Int = ReadPixel (pm, ii, jj)
                rgba = $FFFFFFFF - (rgba & $FFFFFF)
                WritePixel (pmDst, bx * DW + ii, by * DH + jj, rgba)
              Next
            Next

          Next
        Next
        ShadowImage[img] = LoadImage (pmDst)
        Shadow = True
      Next

      SetAlpha (Alpha)
      SetBlend (Blend)
      SetClsColor (ClsColorR, ClsColorG, ClsColorB)
      SetColor (ColorR, ColorG, ColorB)
    End If
  End Method

End Type
