{
  This package contains the Tesseract Open Source OCR Engine.
  Originally developed at Hewlett Packard Laboratories Bristol and
  at Hewlett Packard Co, Greeley Colorado, all the code
  in this distribution is now licensed under the Apache License:

  ** Licensed under the Apache License, Version 2.0 (the "License");
  ** you may not use this file except in compliance with the License.
  ** You may obtain a copy of the License at
  ** http://www.apache.org/licenses/LICENSE-2.0
  ** Unless required by applicable law or agreed to in writing, software
  ** distributed under the License is distributed on an "AS IS" BASIS,
  ** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  ** See the License for the specific language governing permissions and
  ** limitations under the License.
}


 // Pascal bindings for Tesseract 3.02 translated from api/capi.h
 // by Malcolm Poole


unit tesseract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF HAS_LEPTONICA}
  , leptonica
  {$ENDIF};

const
  {$IFDEF LINUX}
  LIBTESS = 'tesseract';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  // this is the DLL contained in http://tesseract-ocr.googlecode.com/files/tesseract-3.02.02-win32-lib-include-dirs.zip
  LIBTESS = 'libtesseract302';
  {$ENDIF}

type

  TessAPIHandle = Pointer;
  TessPageIterator = Pointer;
  TessResultIterator = Pointer;
  TessMutableIterator = Pointer;
  {$IFNDEF HAS_LEPTONICA}
  PLPix = Pointer;
  PPixArray = Pointer;
  PBoxArray = Pointer;
  {$ENDIF}
  PEtextDesc = Pointer;

  TTessOcrEngineMode = (OEM_TESSERACT_ONLY, OEM_CUBE_ONLY, OEM_TESSERACT_CUBE_COMBINED, OEM_DEFAULT);
  TTessPageSegMode   = (PSM_OSD_ONLY, PSM_AUTO_OSD, PSM_AUTO_ONLY, PSM_AUTO, PSM_SINGLE_COLUMN, PSM_SINGLE_BLOCK_VERT_TEXT,
                                     PSM_SINGLE_BLOCK, PSM_SINGLE_LINE, PSM_SINGLE_WORD, PSM_CIRCLE_WORD, PSM_SINGLE_CHAR, PSM_COUNT );
  TTessPageIteratorLevel = (RIL_BLOCK, RIL_PARA, RIL_TEXTLINE, RIL_WORD, RIL_SYMBOL);
  TTessPolyBlockType     = (PT_UNKNOWN, PT_FLOWING_TEXT, PT_HEADING_TEXT, PT_PULLOUT_TEXT, PT_TABLE, PT_VERTICAL_TEXT,
                                     PT_CAPTION_TEXT, PT_FLOWING_IMAGE, PT_HEADING_IMAGE, PT_PULLOUT_IMAGE, PT_HORZ_LINE, PT_VERT_LINE,
                                     PT_NOISE, PT_COUNT );
  TTessOrientation       = (ORIENTATION_PAGE_UP, ORIENTATION_PAGE_RIGHT, ORIENTATION_PAGE_DOWN, ORIENTATION_PAGE_LEFT );
  TessWritingDirection   = (WRITING_DIRECTION_LEFT_TO_RIGHT, WRITING_DIRECTION_RIGHT_TO_LEFT, WRITING_DIRECTION_TOP_TO_BOTTOM );
  TTessTextlineOrder     = (TEXTLINE_ORDER_LEFT_TO_RIGHT, TEXTLINE_ORDER_RIGHT_TO_LEFT, TEXTLINE_ORDER_TOP_TO_BOTTOM);



  // Returns the version identifier as a static string. Do not delete.
  function TessVersion: PChar; cdecl; external LIBTESS;
  procedure TessDeleteText(txt: PChar); cdecl; external LIBTESS;       //NOT TESTED
  procedure TessDeleteTextArray(arr: PPCharArray); cdecl; external LIBTESS;       //NOT TESTED
  procedure TessDeleteIntArray(arr: PIntegerArray); cdecl; external LIBTESS;       //NOT TESTED

  // Creates a new Tesseract instance
  function TessBaseAPICreate: TessAPIHandle; cdecl; external LIBTESS;

  // Deletes the Tesseract instance
  procedure TessBaseAPIDelete(T: TessAPIHandle);cdecl; external LIBTESS;

  // Set the name of the input file. Needed only for training and
  // reading a UNLV zone file.
  procedure TessBaseAPISetInputName( T: TessAPIHandle; ipname: PChar); cdecl; external LIBTESS;       //NOT TESTED

  // Set the name of the bonus output files. Needed only for debugging.
  procedure TessBaseAPISetOutputName( T: TessAPIHandle; opname: PChar); cdecl; external LIBTESS;       //NOT TESTED

  // Set the value of an internal "parameter."
  // Supply the name of the parameter and the value as a string, just as
  // you would in a config file.
  // Returns false if the name lookup failed.
  // Eg SetVariable("tessedit_char_blacklist", "xyz"); to ignore x, y and z.
  // Or SetVariable("classify_bln_numeric_mode", "1"); to set numeric-only mode.
  // SetVariable may be used before Init, but settings will revert to
  // defaults on End().
  // TODO(rays) Add a command-line option to dump the parameters to stdout
  // and add a pointer to it in the FAQ
  //
  // Note: Must be called after Init(). Only works for non-init variables
  // (init variables should be passed to Init()).
  procedure TessBaseAPISetVariable( T: TessAPIHandle; vname, value: PChar); cdecl; external LIBTESS;       //NOT TESTED
  procedure TessBaseAPISetDebugVariable( T: TessAPIHandle; vname, value: PChar); cdecl; external LIBTESS;       //NOT TESTED

  // Returns true if the parameter was found among Tesseract parameters.
  // Fills in value with the value of the parameter.
  procedure TessBaseAPIGetIntVariable( T: TessAPIHandle; vname: PChar; value: PInteger); cdecl; external LIBTESS;       //NOT TESTED
  procedure TessBaseAPIGetBoolVariable( T: TessAPIHandle; vname: PChar; value: PBoolean); cdecl; external LIBTESS;       //NOT TESTED
  procedure TessBaseAPIGetDoubleVariable(T: TessAPIHandle; vname: Pchar; value: PDouble); cdecl; external LIBTESS;       //NOT TESTED

  // Returns the pointer to the string that represents the value of the
  // parameter if it was found among Tesseract parameters.
  function TessBaseAPIGetStringVariable(T: TessAPIHandle; vname: PChar): PChar; cdecl; external LIBTESS;       //NOT TESTED

{
TESS_API void  TESS_CALL TessBaseAPIPrintVariables(      const TessBaseAPI* handle, FILE* fp);
}

  procedure TessBaseAPIPrintVariablesToFile(T: TessAPIHandle; fname: PChar); cdecl; external LIBTESS;


  function TessBaseAPIInit1(T: TessAPIHandle; datapath, language: PChar; oem: TTessOcrEngineMode;
                                          configs: PPChar; configs_size: integer): integer; cdecl; external LIBTESS;       //NOT TESTED
  function TessBaseAPIInit2(T: TessAPIHandle; datapath, language: PChar; oem: TTessOcrEngineMode): integer; cdecl; external LIBTESS;       //NOT TESTED


  // Start tesseract. Returns zero on success and -1 on failure.
  // NOTE that the only members that may be called before Init are those
  // listed above here in the class definition.
  //
  // The datapath must be the name of the parent directory of tessdata and
  // must end in / . Any name after the last / will be stripped.
  // The language is (usually) an ISO 639-3 string or NULL will default to eng.
  // It is entirely safe (and eventually will be efficient too) to call
  // Init multiple times on the same instance to change language, or just
  // to reset the classifier.
  // The language may be a string of the form [~]<lang>[+[~]<lang>]* indicating
  // that multiple languages are to be loaded. Eg hin+eng will load Hindi and
  // English. Languages may specify internally that they want to be loaded
  // with one or more other languages, so the ~ sign is available to override
  // that. Eg if hin were set to load eng by default, then hin+~eng would force
  // loading only hin. The number of loaded languages is limited only by
  // memory, with the caveat that loading additional languages will impact
  // both speed and accuracy, as there is more work to do to decide on the
  // applicable language, and there is more chance of hallucinating incorrect
  // words.
  // WARNING: On changing languages, all Tesseract parameters are reset
  // back to their default values. (Which may vary between languages.)
  function TessBaseAPIInit3(T: TessAPIHandle; datapath, language: PChar): integer; cdecl; external LIBTESS;

  // Returns the languages string used in the last valid initialization.
  // If the last initialization specified "deu+hin" then that will be
  // returned. If hin loaded eng automatically as well, then that will
  // not be included in this list. To find the languages actually
  // loaded use GetLoadedLanguagesAsVector.
  // The returned string should NOT be deleted.
  function TessBaseAPIGetInitLanguagesAsString(T: TessAPIHandle): PChar; cdecl; external LIBTESS;

  // Returns the loaded languages in the vector of STRINGs.
  // Includes all languages loaded by the last Init, including those loaded
  // as dependencies of other loaded languages.
  function TessBaseAPIGetLoadedLanguagesAsVector(T: TessAPIHandle): PPChar; cdecl; external LIBTESS;

  // Returns the available languages in the vector of STRINGs.
  function TessBaseAPIGetAvailableLanguagesAsVector(T: TessAPIHandle): PPChar; cdecl; external LIBTESS;

  // Init only the lang model component of Tesseract. The only functions
  // that work after this init are SetVariable and IsValidWord.
  // WARNING: temporary! This function will be removed from baseapi and placed
  // in a separate API at some future time.
  function TessBaseAPIInitLangMod(T: TessAPIHandle; datapath, language: PChar): Integer; cdecl; external LIBTESS;       //NOT TESTED

  // Init only for page layout analysis. Use only for calls to SetImage and
  // AnalysePage. Calls that attempt recognition will generate an error.
  procedure TessBaseAPIInitForAnalysePage(T: TessAPIHandle); cdecl; external LIBTESS;

  // Read a "config" file containing a set of param, value pairs.
  // Searches the standard places: tessdata/configs, tessdata/tessconfigs
  // and also accepts a relative or absolute path name.
  // Note: only non-init params will be set (init params are set by Init()).
  procedure TessBaseAPIReadConfigFile(T: TessAPIHandle; filename: PChar); cdecl; external LIBTESS;       //NOT TESTED
  //Same as above, but only set debug params from the given config file.
  procedure TessBaseAPIReadDebugConfigFile(T: TessAPIHandle; filename: PChar); cdecl; external LIBTESS;       //NOT TESTED

  // Set the current page segmentation mode. Defaults to PSM_SINGLE_BLOCK.
  // The mode is stored as an IntParam so it can also be modified by
  // ReadConfigFile or SetVariable("tessedit_pageseg_mode", mode as string).
  procedure TessBaseAPISetPageSegMode(T: TessAPIHandle; mode: TTessPageSegMode); cdecl; external LIBTESS;

  // Return the current page segmentation mode.
  function TessBaseAPIGetPageSegMode(T: TessAPIHandle): TTessPageSegMode; cdecl; external LIBTESS;

  function TessBaseAPIRect(T: TessAPIHandle; imagedata: PByte;
                                         bytes_per_pixel, bytes_per_line,
                                         left, top, width, height: Integer): PChar; cdecl; external LIBTESS;       //NOT TESTED

  procedure TessBaseAPIClearAdaptiveClassifier(T: TessAPIHandle); cdecl; external LIBTESS;       //NOT TESTED

  procedure TessBaseAPISetImage(T: TessAPIHandle; imagedata: PByte; width, height,
                                             bytes_per_pixel, bytes_per_line: Integer); cdecl; external LIBTESS;       //NOT TESTED

  // Provide an image for Tesseract to recognize. Format is as
  // TesseractRect above. Does not copy the image buffer, or take
  // ownership. The source image may be destroyed after Recognize is called,
  // either explicitly or implicitly via one of the Get*Text functions.
  // SetImage clears all recognition results, and sets the rectangle to the
  // full image, so it may be followed immediately by a GetUTF8Text, and it
  // will automatically perform recognition.
  procedure TessBaseAPISetImage2(T: TessAPIHandle; pix: PLPix); cdecl; external LIBTESS;       //NOT TESTED

  procedure TessBaseAPISetSourceResolution(T: TessAPIHandle; ppi: Integer); cdecl; external LIBTESS;       //NOT TESTED

  // Restrict recognition to a sub-rectangle of the image. Call after SetImage.
  // Each SetRectangle clears the recogntion results so multiple rectangles
  // can be recognized with the same image.
  procedure TessBaseAPISetRectangle(T: TessAPIHandle; left, top, width, height: Integer); cdecl; external LIBTESS;       //NOT TESTED

  // Get a copy of the internal thresholded image from Tesseract.
  // Caller takes ownership of the Pix and must pixDestroy it.
  // May be called any time after SetImage, or after TesseractRect.
  function TessBaseAPIGetThresholdedImage( T:  TessAPIHandle): PLPix; cdecl; external LIBTESS;       //NOT TESTED

  function TessBaseAPIGetRegions(T: TessAPIHandle; pixa: PPixArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED
  function TessBaseAPIGetTextlines(T: TessAPIHandle; pixa: PPixArray; blockids: PIntegerArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED
  function TessBaseAPIGetStrips(T: TessAPIHandle; pixa: PPixArray; blockids: PIntegerArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED

  // Get the words as a leptonica-style
  // Boxa, Pixa pair, in reading order.
  // Can be called before or after Recognize.
  function TessBaseAPIGetWords(T: TessAPIHandle; pixa: PPixArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED
  function TessBaseAPIGetConnectedComponents(T: TessAPIHandle; cc: PPixArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED
  function TessBaseAPIGetComponentImages(T: TessAPIHandle; level: TTessPageIteratorLevel; text_only: Boolean;
                                            pixa: PPixArray; blockids: PIntegerArray): PBoxArray; cdecl; external LIBTESS;       //NOT TESTED

  function TessBaseAPIGetThresholdedImageScaleFactor(T: TessAPIHandle): Integer; cdecl; external LIBTESS;       //NOT TESTED

  // Dump the internal binary image to a PGM file.
  // @deprecated Use GetThresholdedImage and write the image using pixWrite
  // instead if possible.
  procedure TessBaseAPIDumpPGM(T: TessAPIHandle; filename: PChar); cdecl; external LIBTESS;       //NOT TESTED

  // Runs page layout analysis in the mode set by SetPageSegMode.
  // May optionally be called prior to Recognize to get access to just
  // the page layout results. Returns an iterator to the results.
  // Returns NULL on error.
  // The returned iterator must be deleted after use.
  // WARNING! This class points to data held within the TessBaseAPI class, and
  // therefore can only be used while the TessBaseAPI class still exists and
  // has not been subjected to a call of Init, SetImage, Recognize, Clear, End
  // DetectOS, or anything else that changes the internal PAGE_RES.
  function TessBaseAPIAnalyseLayout(T: TessAPIHandle): TessPageIterator; cdecl; external LIBTESS;

  // Recognize the image from SetAndThresholdImage, generating Tesseract
  // internal structures. Returns 0 on success.
  // Optional. The Get*Text functions below will call Recognize if needed.
  // After Recognize, the output is kept internally until the next SetImage.
  function TessBaseAPIRecognize(T: TessAPIHandle; monitor: PEtextDesc): Integer; cdecl; external LIBTESS;

  // Variant on Recognize used for testing chopper.
  function TessBaseAPIRecognizeForChopTest(T: TessAPIHandle; monitor: PEtextDesc): Integer; cdecl; external LIBTESS;


  // Recognizes all the pages in the named file, as a multi-page tiff or
  // list of filenames, or single image, and gets the appropriate kind of text
  // according to parameters: tessedit_create_boxfile,
  // tessedit_make_boxes_from_boxes, tessedit_write_unlv, tessedit_create_hocr.
  // Calls ProcessPage on each page in the input file, which may be a
  // multi-page tiff, single-page other file format, or a plain text list of
  // images to read. If tessedit_page_number is non-negative, processing begins
  // at that page of a multi-page tiff file, or filelist.
  // The text is returned in text_out. Returns false on error.
  // If non-zero timeout_millisec terminates processing after the timeout on
  // a single page.
  // If non-NULL and non-empty, and some page fails for some reason,
  // the page is reprocessed with the retry_config config file. Useful
  // for interactively debugging a bad page.
  function TessBaseAPIProcessPages(T: TessAPIHandle; filename, retry_config: PChar; timeout_millisec: integer): PChar; cdecl; external LIBTESS;

  // Recognizes a single page for ProcessPages, appending the text to text_out.
  // The pix is the image processed - filename and page_index are metadata
  // used by side-effect processes, such as reading a box file or formatting
  // as hOCR.
  // If non-zero timeout_millisec terminates processing after the timeout.
  // If non-NULL and non-empty, and some page fails for some reason,
  // the page is reprocessed with the retry_config config file. Useful
  // for interactively debugging a bad page.
  // The text is returned in text_out. Returns false on error.
  function TessBaseAPIProcessPage(T: TessAPIHandle; pix: PLPix; page_index: Integer;
                                            filename, retry_config: PChar; timeout_millisec: integer): PChar; cdecl; external LIBTESS;


  // Get a reading-order iterator to the results of LayoutAnalysis and/or
  // Recognize. The returned iterator must be deleted after use.
  // WARNING! This class points to data held within the TessBaseAPI class, and
  // therefore can only be used while the TessBaseAPI class still exists and
  // has not been subjected to a call of Init, SetImage, Recognize, Clear, End
  // DetectOS, or anything else that changes the internal PAGE_RES.
  function TessBaseAPIGetIterator(T: TessAPIHandle): TessResultIterator;  cdecl; external LIBTESS;       //NOT TESTED

  // Get a mutable iterator to the results of LayoutAnalysis and/or Recognize.
  // The returned iterator must be deleted after use.
  // WARNING! This class points to data held within the TessBaseAPI class, and
  // therefore can only be used while the TessBaseAPI class still exists and
  // has not been subjected to a call of Init, SetImage, Recognize, Clear, End
  // DetectOS, or anything else that changes the internal PAGE_RES.
  function TessBaseAPIGetMutableIterator(T: TessAPIHandle ): TessMutableIterator; cdecl; external LIBTESS;       //NOT TESTED

  // The recognized text is returned as a char* which is coded
  // as UTF8 and must be freed with the delete [] operator.
  function TessBaseAPIGetUTF8Text(T: TessAPIHandle): PChar; cdecl; external LIBTESS;

  // Make a HTML-formatted string with hOCR markup from the internal
  // data structures.
  // page_number is 0-based but will appear in the output as 1-based.
  function TessBaseAPIGetHOCRText(T: TessAPIHandle; pagenum: integer): PChar; cdecl; external LIBTESS;

   // The recognized text is returned as a char// which is coded in the same
   // format as a box file used in training. Returned string must be freed with
   // the delete [] operator.
   // Constructs coordinates in the original image - not just the rectangle.
   // page_number is a 0-based page index that will appear in the box file.
  function TessBaseAPIGetBoxText(T: TessAPIHandle; page_number: Integer): PChar; cdecl; external LIBTESS;       //NOT TESTED

   // The recognized text is returned as a char// which is coded
   // as UNLV format Latin-1 with specific reject and suspect codes
   // and must be freed with the delete [] operator.
  function TessBaseAPIGetUNLVText(T: TessAPIHandle): PChar; cdecl; external LIBTESS;       //NOT TESTED

  //Returns the (average) confidence value between 0 and 100.
  function TessBaseAPIMeanTextConf(T: TessAPIHandle): Integer; cdecl; external LIBTESS;       //NOT TESTED

  // Returns all word confidences (between 0 and 100) in an array, terminated
  // by -1.  The calling function must delete [] after use.
  // The number of confidences should correspond to the number of space-
  // delimited words in GetUTF8Text.
  function TessBaseAPIAllWordConfidences(T: TessAPIHandle): PIntegerArray; cdecl; external LIBTESS;       //NOT TESTED

  // Applies the given word to the adaptive classifier if possible.
  // The word must be SPACE-DELIMITED UTF-8 - l i k e t h i s , so it can
  // tell the boundaries of the graphemes.
  // Assumes that SetImage/SetRectangle have been used to set the image
  // to the given word. The mode arg should be PSM_SINGLE_WORD or
  // PSM_CIRCLE_WORD, as that will be used to control layout analysis.
  // The currently set PageSegMode is preserved.
  // Returns false if adaption was not possible for some reason.
  function TessBaseAPIAdaptToWordStr(T: TessAPIHandle; mode: TTessPageSegMode; wordstr: PChar): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  // Free up recognition results and any stored image data, without actually
  // freeing any recognition data that would be time-consuming to reload.
  // Afterwards, you must call SetImage or TesseractRect before doing
  // any Recognize or Get// operation.
  procedure TessBaseAPIClear(T: TessAPIHandle); cdecl; external LIBTESS;       //NOT TESTED

  // Close down tesseract and free up all memory. End() is equivalent to
  // destructing and reconstructing your TessBaseAPI.
  // Once End() has been used, none of the other API functions may be used
  // other than Init and anything declared above it in the class definition.
  procedure TessBaseAPIEnd(T: TessAPIHandle); cdecl; external LIBTESS;       //NOT TESTED

  // Check whether a word is valid according to Tesseract's language model
  // @return 0 if the word is invalid, non-zero if valid.
  // @warning temporary! This function will be removed from capi and placed
  // in a separate API at some future time.
  function TessBaseAPIIsValidWord(T: TessAPIHandle; aword: PChar): Integer; cdecl; external LIBTESS;       //NOT TESTED

  function TessBaseAPIGetTextDirection(T: TessAPIHandle; out_offset: PInteger; out_slope: PSingle): Boolean; cdecl; external LIBTESS;       //NOT TESTED

{
#ifdef TESS_CAPI_INCLUDE_BASEAPI
TESS_API void  TESS_CALL TessBaseAPISetDictFunc(TessBaseAPI* handle, TessDictFunc f);
TESS_API void  TESS_CALL TessBaseAPISetProbabilityInContextFunc(TessBaseAPI* handle, TessProbabilityInContextFunc f);
TESS_API void  TESS_CALL TessBaseAPISetFillLatticeFunc(TessBaseAPI* handle, TessFillLatticeFunc f);
function  TessBaseAPIDetectOS(TessBaseAPI* handle, OSResults* results);

TESS_API void  TESS_CALL TessBaseAPIGetFeaturesForBlob(TessBaseAPI* handle, TBLOB* blob, const DENORM* denorm, INT_FEATURE_ARRAY int_features,
                                                       int* num_features, int* FeatureOutlineIndex);

TESS_API ROW*  TESS_CALL TessFindRowForBox(BLOCK_LIST* blocks, int left, int top, int right, int bottom);
TESS_API void  TESS_CALL TessBaseAPIRunAdaptiveClassifier(TessBaseAPI* handle, TBLOB* blob, const DENORM* denorm, int num_max_matches,
                                                          int* unichar_ids, float* ratings, int* num_matches_returned);
#endif
}

  function TessBaseAPIGetUnichar(T: TessAPIHandle; unichar_id: Integer): PChar; cdecl; external LIBTESS;       //NOT TESTED

  {
  #ifdef TESS_CAPI_INCLUDE_BASEAPI
TESS_API const TessDawg*
               TESS_CALL TessBaseAPIGetDawg(const TessBaseAPI* handle, int i);
TESS_API int   TESS_CALL TessBaseAPINumDawgs(const TessBaseAPI* handle);
#endif

#ifdef TESS_CAPI_INCLUDE_BASEAPI
TESS_API ROW*  TESS_CALL TessMakeTessOCRRow(float baseline, float xheight, float descender, float ascender);
TESS_API TBLOB*
               TESS_CALL TessMakeTBLOB(Pix *pix);
TESS_API void  TESS_CALL TessNormalizeTBLOB(TBLOB *tblob, ROW *row, BOOL numeric_mode, DENORM *denorm);

TESS_API TessOcrEngineMode
               TESS_CALL TessBaseAPIOem(const TessBaseAPI* handle);
TESS_API void  TESS_CALL TessBaseAPIInitTruthCallback(TessBaseAPI* handle, TessTruthCallback *cb);

TESS_API TessCubeRecoContext*
               TESS_CALL TessBaseAPIGetCubeRecoContext(const TessBaseAPI* handle);
#endif

TESS_API void  TESS_CALL TessBaseAPISetMinOrientationMargin(TessBaseAPI* handle, double margin);
#ifdef TESS_CAPI_INCLUDE_BASEAPI
TESS_API void  TESS_CALL TessBaseGetBlockTextOrientations(TessBaseAPI* handle, int** block_orientation, bool** vertical_writing);

TESS_API BLOCK_LIST*
               TESS_CALL TessBaseAPIFindLinesCreateBlockList(TessBaseAPI* handle);
#endif
}

{ Page iterator }

procedure  TessPageIteratorDelete(pihandle: TessPageIterator); cdecl; external LIBTESS;       //NOT TESTED

function TessPageIteratorCopy(pihandle: TessPageIterator): TessPageIterator; cdecl; external LIBTESS;       //NOT TESTED

  // Moves the iterator to point to the start of the page to begin an
  // iteration.
  procedure  TessPageIteratorBegin(pihandle: TessPageIterator); cdecl; external LIBTESS;       //NOT TESTED

  // Moves to the start of the next object at the given level in the
  // page hierarchy, and returns false if the end of the page was reached.
  // NOTE that RIL_SYMBOL will skip non-text blocks, but all other
  // PageIteratorLevel level values will visit each non-text block once.
  // Think of non text blocks as containing a single para, with a single line,
  // with a single imaginary word.
  // Calls to Next with different levels may be freely intermixed.
  // This function iterates words in right-to-left scripts correctly, if
  // the appropriate language has been loaded into Tesseract.
  function TessPageIteratorNext(pihandle: TessPageIterator; level: TTessPageIteratorLevel): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  // Returns true if the iterator is at the start of an object at the given
  // level.
  //
  // For instance, suppose an iterator it is pointed to the first symbol of the
  // first word of the third line of the second paragraph of the first block in
  // a page, then:
  //   it.IsAtBeginningOf(RIL_BLOCK) = false
  //   it.IsAtBeginningOf(RIL_PARA) = false
  //   it.IsAtBeginningOf(RIL_TEXTLINE) = true
  //   it.IsAtBeginningOf(RIL_WORD) = true
  //   it.IsAtBeginningOf(RIL_SYMBOL) = true
  function TessPageIteratorIsAtBeginningOf(pihandle: TessPageIterator; level: TTessPageIteratorLevel): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  // Returns whether the iterator is positioned at the last element in a
  // given level. (e.g. the last word in a line, the last line in a block)
  //
  //     Here's some two-paragraph example
  //   text.  It starts off innocuously
  //   enough but quickly turns bizarre.
  //     The author inserts a cornucopia
  //   of words to guard against confused
  //   references.
  //
  // Now take an iterator it pointed to the start of "bizarre."
  //  it.IsAtFinalElement(RIL_PARA, RIL_SYMBOL) = false
  //  it.IsAtFinalElement(RIL_PARA, RIL_WORD) = true
  //  it.IsAtFinalElement(RIL_BLOCK, RIL_WORD) = false
  function TessPageIteratorIsAtFinalElement(pihandle: TessPageIterator; level, element: TTessPageIteratorLevel): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  // ============= Accessing data ==============.
  // Coordinate system:
  // Integer coordinates are at the cracks between the pixels.
  // The top-left corner of the top-left pixel in the image is at (0,0).
  // The bottom-right corner of the bottom-right pixel in the image is at
  // (width, height).
  // Every bounding box goes from the top-left of the top-left contained
  // pixel to the bottom-right of the bottom-right contained pixel, so
  // the bounding box of the single top-left pixel in the image is:
  // (0,0)->(1,1).
  // If an image rectangle has been set in the API, then returned coordinates
  // relate to the original (full) image, rather than the rectangle.

  // Returns the bounding rectangle of the current object at the given level.
  // See comment on coordinate system above.
  // Returns false if there is no such object at the current position.
  // The returned bounding box is guaranteed to match the size and position
  // of the image returned by GetBinaryImage, but may clip foreground pixels
  // from a grey image. The padding argument to GetImage can be used to expand
  // the image to include more foreground pixels. See GetImage below.
  function TessPageIteratorBoundingBox(pihandle: TessPageIterator; level: TTessPageIteratorLevel;
                                                     left, top, right, bottom: PInteger): Boolean; cdecl; external LIBTESS;       //NOT TESTED
  //Returns the type of the current block.
  function TessPageIteratorBlockType(pihandle: TessPageIterator): TTessPolyBlockType; cdecl; external LIBTESS;       //NOT TESTED

  // Returns a binary image of the current object at the given level.
  // The position and size match the return from BoundingBoxInternal, and so
  // this could be upscaled with respect to the original input image.
  // Use pixDestroy to delete the image after use.
  function TessPageIteratorGetBinaryImage(pihandle: TessPageIterator; level: TTessPageIteratorLevel): PLPix; cdecl; external LIBTESS;       //NOT TESTED

  // Returns an image of the current object at the given level in greyscale
  // if available in the input. To guarantee a binary image use BinaryImage.
  // NOTE that in order to give the best possible image, the bounds are
  // expanded slightly over the binary connected component, by the supplied
  // padding, so the top-left position of the returned image is returned
  // in (left,top). These will most likely not match the coordinates
  // returned by BoundingBox.
  // Use pixDestroy to delete the image after use.
  function TessPageIteratorGetImage(pihandle: TessPageIterator; level: TTessPageIteratorLevel;
                                                     padding: Integer; left, top: PInteger): PLPix; cdecl; external LIBTESS;       //NOT TESTED

  // Returns the baseline of the current object at the given level.
  // The baseline is the line that passes through (x1, y1) and (x2, y2).
  // WARNING: with vertical text, baselines may be vertical!
  // Returns false if there is no baseline at the current position.
  function TessPageIteratorBaseline(pihandle: TessPageIterator; level: TTessPageIteratorLevel;
                                                  x1, y1, x2, y2: PInteger): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  // Returns orientation for the block the iterator points to.
  //   orientation, writing_direction, textline_order: see publictypes.h
  //   deskew_angle: after rotating the block so the text orientation is
  //                 upright, how many radians does one have to rotate the
  //                 block anti-clockwise for it to be level?
  //                   -Pi/4 <= deskew_angle <= Pi/4
  procedure TessPageIteratorOrientation(pihandle: TessPageIterator; orientation: TTessOrientation; writingdirection: TessWritingDirection;
                                                  textlineorder: TTessTextlineOrder; deskew_angle: PSingle); cdecl; external LIBTESS;       //NOT TESTED

  { Result iterator }

  procedure TessResultIteratorDelete(rihandle: TessResultIterator); cdecl; external LIBTESS;       //NOT TESTED

  function TessResultIteratorCopy(rihandle: TessResultIterator): TessResultIterator; cdecl; external LIBTESS;       //NOT TESTED

  function TessResultIteratorGetPageIterator(rihandle: TessResultIterator): TessPageIterator; cdecl; external LIBTESS;       //NOT TESTED

  function TessResultIteratorGetPageIteratorConst(rihandle: TessResultIterator): TessPageIterator; cdecl; external LIBTESS;       //NOT TESTED

  // Returns the null terminated UTF-8 encoded text string for the current
  // object at the given level. Use TessDeleteText to free after use.
  function TessResultIteratorGetUTF8Text(riHandle: TessResultIterator; level: TTessPageIteratorLevel): PChar; cdecl; external LIBTESS;       //NOT TESTED

  function TessResultIteratorConfidence(riHandle: TessResultIterator; level: TTessPageIteratorLevel): Single; cdecl; external LIBTESS;       //NOT TESTED


  function TessResultIteratorWordFontAttributes(riHandle: TessResultIterator; is_bold, is_italic,
                                                              is_underlined, is_monospace, is_serif,
                                                              is_smallcaps: PBoolean; pointsize, font_id: PInteger): PChar; cdecl; external LIBTESS;       //NOT TESTED

  function TessResultIteratorWordIsFromDictionary(piHandle: TessResultIterator): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  function  TessResultIteratorWordIsNumeric(piHandle: TessResultIterator): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  function  TessResultIteratorSymbolIsSuperscript(piHandle: TessResultIterator): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  function  TessResultIteratorSymbolIsSubscript(piHandle: TessResultIterator): Boolean; cdecl; external LIBTESS;       //NOT TESTED

  function  TessResultIteratorSymbolIsDropcap(piHandle: TessResultIterator): Boolean; cdecl; external LIBTESS;       //NOT TESTED



implementation

end.
