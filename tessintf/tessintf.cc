/**   This version compiles with tesseract v. 3.00   **/

#include "tessintf.h"
#include "/usr/local/include/tesseract/baseapi.h"
#include <stdio.h>
using namespace std;
using namespace tesseract;

extern "C"{

  /**
   * Creates a new instance of TessBaseAPI and initialises it.
   * datapath and language may be NULL, in which case defaults are used
   * Returns a handle to the TessBaseAPI instance.
   */
tessHandle EXPORTCALL tesseract_new(const char* datapath, const char* language){
TessBaseAPI* Tess = new TessBaseAPI;
if (Tess->Init(datapath, language, NULL, 0, 1)!=0){
  delete Tess;
  return NULL;
  }
else {
  return Tess;
  }
};

  /**
   * Destroys an instance of TessBaseAPI created by tesseract_new
   */
extern void EXPORTCALL tesseract_destroy(tessHandle APIHandle){
  delete APIHandle;
}

  /**
   * Provides more controlled form of initialising tesseract than performed
   * in tesseract_new
   *  
   * Eventually instances will be thread-safe and totally independent,
   * but for now, they all point to the same underlying engine,
   * and are NOT RE-ENTRANT OR THREAD-SAFE. For now:
   * it is safe to Init multiple TessBaseAPIs in the same language, use them
   * sequentially, and End or delete them all, but once one is Ended, you can't
   * do anything other than End the others. After End, it is safe to Init
   * again on the same one.
   *
   * The datapath must be the name of the data directory (no ending /) or
   * some other file in which the data directory resides (for instance argv[0].)
   * The language is (usually) an ISO 639-3 string or NULL will default to eng.
   * It is entirely safe (and eventually will be efficient too) to call
   * Init multiple times on the same instance to change language, or just
   * to reset the classifier.
   * WARNING: On changing languages, all Variables are reset back to their
   * default values. If you have a rare need to set a Variable that controls
   * initialization for a second call to Init you should explicitly
   * call End() and then use SetVariable before Init. This is only a very
   * rare use case, since there are very few uses that require any variables
   * to be set before Init.
   */
extern int EXPORTCALL tesseract_init(tessHandle APIHandle, const char* datapath, const char* language,
           char **configs, int configs_size, bool configs_global_only){
  return APIHandle->Init(datapath, language, configs, configs_size, configs_global_only);
}

  /**
   * Read a "config" file containing a set of variable, value pairs.
   * Searches the standard places: tessdata/configs, tessdata/tessconfigs
   * and also accepts a relative or absolute path name.
   */
extern void EXPORTCALL tesseract_ReadConfigFile(tessHandle APIHandle, const char* filename, bool global_only){
  APIHandle->ReadConfigFile(filename, global_only);
}

  /**
   * Provide an image for Tesseract to recognize. 
   * Tesseract doesn't take a copy or ownership or pixDestroy the image, so
   * it must persist until after Recognize.
   */
extern void EXPORTCALL tesseract_SetImage(tessHandle APIHandle, const Pix* pix){
   APIHandle->SetImage( pix );
}

  /**
   * Restrict recognition to a sub-rectangle of the image. Call after SetImage.
   * Each SetRectangle clears the recogntion results so multiple rectangles
   * can be recognized with the same image.
   */
extern void EXPORTCALL tesseract_SetRectangle(tessHandle APIHandle, int left, int top, int width, int height){
   APIHandle->SetRectangle(left, top, width, height);
}

  /**
   * The recognized text is returned as a char* which is coded
   * as UTF8 and must be freed with tesseract_DeleteString.
   */
extern char* EXPORTCALL tesseract_GetUTF8Text(tessHandle APIHandle){
   return APIHandle->GetUTF8Text();
}

  /**
   * Get a copy of the internal thresholded image from Tesseract.
   * Caller takes ownership of the Pix and must pixDestroy it.
   * May be called any time after SetImage, or after TesseractRect.
   */
extern Pix* EXPORTCALL tesseract_GetThresholdedImage(tessHandle APIHandle){
  return APIHandle->GetThresholdedImage();
}

  /**
   * Free up recognition results and any stored image data, without actually
   * freeing any recognition data that would be time-consuming to reload.
   * Afterwards, you must call SetImage or TesseractRect before doing
   * any Recognize or Get* operation.
   */
extern void EXPORTCALL tesseract_Clear(tessHandle APIHandle){
  APIHandle->Clear();
}

  /**
   * Call between pages or documents etc to free up memory and forget
   * adaptive data.
   */
extern void EXPORTCALL tesseract_ClearAdaptiveClassifier(tessHandle APIHandle){
  APIHandle->ClearAdaptiveClassifier();
}

  /**
   * Set the value of an internal "variable" (of either old or new types).
   * Supply the name of the variable and the value as a string, just as
   * you would in a config file.
   * Returns false if the name lookup failed.
   * SetVariable may be used before Init, to set things that control
   * initialization, but note that on End all settings are lost and
   * the next Init will use the defaults unless SetVariable is used again.
   */
extern bool EXPORTCALL tesseract_SetVariable(tessHandle APIHandle, const char* variable, const char* value){
   return APIHandle->SetVariable(variable, value) ;
}

  /**
   * Set the current page segmentation mode. Defaults to PSM_AUTO.
   *The mode is stored as an INT_VARIABLE so it can also be modified by
   * ReadConfigFile or SetVariable("tessedit_pageseg_mode", mode as string).
   */
extern void EXPORTCALL tesseract_SetPageSegMode(tessHandle APIHandle, int mode){
   PageSegMode segmode = (PageSegMode)mode;
   APIHandle->SetPageSegMode(segmode);
}

  /** Return the current page segmentation mode. */
 extern int EXPORTCALL tesseract_GetPageSegMode(tessHandle APIHandle){
   return (int)APIHandle->GetPageSegMode();
}

  /**
   * Set the hint for trading accuracy against speed.
   * Default is AVS_FASTEST, which is the old behaviour.
   * Note that this is only a hint. Depending on the language and/or
   * build configuration, speed and accuracy may not be tradeable.
   * Also note that despite being an enum, any value in the range
   * AVS_FASTEST to AVS_MOST_ACCURATE can be provided, and may or may not
   * have an effect, depending on the implementation.
   * The mode is stored as an INT_VARIABLE so it can also be modified by
   * ReadConfigFile or SetVariable("tessedit_accuracyvspeed", mode as string).
   */
extern void EXPORTCALL tesseract_SetAccuracyVSpeed(tessHandle APIHandle, int mode){
  APIHandle->SetAccuracyVSpeed((AccuracyVSpeed)mode);
}

  /**
   * Get the result of page layout analysis as a leptonica-style
   * Boxa, Pixa pair, in reading order.
   * Can be called before or after Recognize.
   */
extern Boxa* EXPORTCALL tesseract_GetRegions(tessHandle APIHandle, Pixa** pixa){
  return APIHandle->GetRegions(pixa);
}

  /**
   * Get the textlines as a leptonica-style
   * Boxa, Pixa pair, in reading order.
   * Can be called before or after Recognize.
   * If blockids is not NULL, the block-id of each line is also returned as an
   * array of one element per line. delete [] after use.
   */
extern Boxa* EXPORTCALL tesseract_GetTextLines(tessHandle APIHandle, Pixa** pixa, int** blockids){
   return APIHandle->GetTextlines(pixa, blockids);
}

  /**
   * Get the words as a leptonica-style
   * Boxa, Pixa pair, in reading order.
   * Can be called before or after Recognize.
   */
extern Boxa* EXPORTCALL tesseract_GetWords(tessHandle APIHandle, Pixa** pixa){
   return APIHandle->GetWords(pixa);
}

    /**
   * Make a HTML-formatted string with hOCR markup from the internal
   * data structures.
   * STL removed from original patch submission and refactored by rays.
   * page_id is 1-based and will appear in the output.
   */
 extern char* EXPORTCALL tesseract_GetHOCRText(tessHandle APIHandle, int page_id){
    return APIHandle->GetHOCRText(page_id);
 }

  /**
   * The recognized text is returned as a char* which is coded in the same
   * format as a box file used in training. Returned string must be freed with
   * tesseract_DeleteString.
   * Constructs coordinates in the original image - not just the rectangle.
   * page_number is a 0-base page index that will appear in the box file.
   */
 extern char* EXPORTCALL tesseract_GetBoxText(tessHandle APIHandle, int page_number){
    return APIHandle->GetBoxText(page_number);
 }

  /**
   * The recognized text is returned as a char* which is coded
   * as UNLV format Latin-1 with specific reject and suspect codes
   * and must be freed with tesseract_DeleteString.
   */
extern char* EXPORTCALL tesseract_GetUNLVText(tessHandle APIHandle){
   return APIHandle->GetUNLVText();
}

  /**
   * Deletes a char array returned by tesseract_Get*Text
   */
extern void EXPORTCALL tesseract_DeleteString(char* pCstr){
  delete[] pCstr;  
  pCstr=0;
}


  /** Returns the (average) confidence value between 0 and 100. */
extern int EXPORTCALL tesseract_MeanTextConf(tessHandle APIHandle){
   return APIHandle->MeanTextConf();
}

  /**
   * Returns all word confidences (between 0 and 100) in an array, terminated
   * by -1.  The calling function must delete the result with
   * tesseract_DeleteWordConfidences after use.
   * The number of confidences should correspond to the number of space-
   * delimited words in GetUTF8Text.
   */
extern int* EXPORTCALL tesseract_AllWordConfidences(tessHandle APIHandle){
   return APIHandle->AllWordConfidences();
}

  /**
   * Deletes a value returned by tesseract_AllWordConfidences
   */
extern void EXPORTCALL tesseract_DeleteWordConfidences(int* pconf){
  delete[] pconf;  
  pconf=0;
}


}

