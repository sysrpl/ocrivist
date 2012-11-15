///////////////////////////////////////////////////////////////////////
// File:        tessintf.pas
// Description: Pascal bindings for libtessintf
//              Libtessintf is a C wrapper for the Tesseract API
// Author:      Malcolm Poole
//
//    ***  This version for use with Tesseract v. 3.00   ***
//
// The Tesseract API is (C) Copyright 2006, Google Inc.
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
///////////////////////////////////////////////////////////////////////

unit tessintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, leptonica;

type
  TTesseract = Pointer;
{  TBoxa = Pointer;
  TPixa = Pointer;
  PPixa = ^TPixa;}
  PPInteger = ^PInteger;

  TPageSegMode = (
    PSM_AUTO,           ///< Fully automatic page segmentation.
    PSM_SINGLE_COLUMN,  ///< Assume a single column of text of variable sizes.
    PSM_SINGLE_BLOCK,   ///< Assume a single uniform block of text. (Default.)
    PSM_SINGLE_LINE,    ///< Treat the image as a single text line.
    PSM_SINGLE_WORD,    ///< Treat the image as a single word.
    PSM_SINGLE_CHAR );    ///< Treat the image as a single character.

const
    PSM_COUNT  =  6;         ///< Number of values in TPageSegMode

    AVS_FASTEST = 0;         ///< Fastest speed, but lowest accuracy.
    AVS_MOST_ACCURATE = 100;

const
  LIBTESSINTF = 'tessintf';

function tesseract_new( datapath, language: PChar): TTesseract; stdcall; external LIBTESSINTF;

procedure tesseract_destroy( Tess: TTesseract ); stdcall; external LIBTESSINTF;

function tesseract_init( APIHandle: TTesseract; datapath: PChar; language: PChar;
                         {char **configs} configs: Pointer; configs_size: Integer; configs_global_only: Boolean): Integer; stdcall; external LIBTESSINTF;

procedure tesseract_SetImage(APIHandle: TTesseract; pix: PLPix ); stdcall; external LIBTESSINTF;

function tesseract_GetUTF8Text( APIHandle: TTesseract): PChar; stdcall; external LIBTESSINTF;

function tesseract_GetBoxText( APIHandle: TTesseract; page_number: Integer ): PChar; stdcall; external LIBTESSINTF;

procedure tesseract_SetRectangle( APIHandle: TTesseract; left, top, width, height: Integer); stdcall; external LIBTESSINTF;

procedure tesseract_SetPageSegMode( APIHandle: TTesseract; mode: TPageSegMode ); stdcall; external LIBTESSINTF;

function tesseract_GetPageSegMode( APIHandle: TTesseract ): TPageSegMode; stdcall; external LIBTESSINTF;

procedure tesseract_SetAccuracyVSpeed( APIHandle: TTesseract; mode: Integer ); stdcall; external LIBTESSINTF;

function tesseract_GetRegions( APIHandle: TTesseract; pixa: PPixArray): TBoxArray; stdcall; external LIBTESSINTF;

function tesseract_GetTextLines( APIHandle: TTesseract; pixa: PPixArray; blockids: PIntegerArray): TBoxArray; stdcall; external LIBTESSINTF;

function tesseract_GetWords( APIHandle: TTesseract; pixa: PPixArray ): PBoxArray; stdcall; external LIBTESSINTF;

function tesseract_GetHOCRText( APIHandle: TTesseract; page_id: Integer ): PChar; stdcall; external LIBTESSINTF;

function tesseract_GetUNLVText( APIHandle: TTesseract ): PChar; stdcall; external LIBTESSINTF;

procedure tesseract_DeleteString(pCstr: PChar); stdcall; external LIBTESSINTF;

function tesseract_MeanTextConf( APIHandle: TTesseract ): Integer; stdcall; external LIBTESSINTF;

function tesseract_AllWordConfidences( APIHandle: TTesseract ): PInteger; stdcall; external LIBTESSINTF;

procedure tesseract_DeleteWordConfidences( pconf: PInteger); stdcall; external LIBTESSINTF;


implementation

end.

