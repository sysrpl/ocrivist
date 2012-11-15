///////////////////////////////////////////////////////////////////////
// File:        tessintf.h
// Description: C interface to the tesseract API .
// Author:      Malcolm Poole <mgpoole@
//
/////   **   This version compiles with tesseract v. 3.00       **
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
#ifndef TESSINTF_H
#define TESSINTF_H

#ifdef __cplusplus
#include "/usr/local/include/tesseract/baseapi.h"
#define EXPORTCALL __attribute__((stdcall))
typedef tesseract::TessBaseAPI *tessHandle;
#else
typedef int bool;
typedef struct tesseract_t *tessHandle;
typedef void* Pix ;
typedef void* Boxa ;
typedef void* Pixa ;
typedef int PageSegMode;
#define EXPORTCALL
#endif

#define DEFAULT_CRYPTO_SIZE 10240

#ifdef __cplusplus

extern "C"
{
#endif


 /** 
  * NOTE: tessintf.cc includes documentation for all of the following methods 
  **/

typedef unsigned int U_Int;

extern tessHandle EXPORTCALL tesseract_new(const char* datapath, const char* language);

extern void EXPORTCALL tesseract_destroy(tessHandle APIHandle);

extern int EXPORTCALL tesseract_init(tessHandle APIHandle, const char* datapath, const char* language,
           char **configs, int configs_size, bool configs_global_only);
	   
extern void EXPORTCALL tesseract_ReadConfigFile(tessHandle APIHandle, const char* filename, bool global_only);

extern void EXPORTCALL tesseract_SetImage(tessHandle APIHandle, const Pix* pix);

extern void EXPORTCALL tesseract_SetRectangle(tessHandle APIHandle, int left, int top, int width, int height);

extern char* EXPORTCALL tesseract_GetUTF8Text(tessHandle APIHandle);

extern Pix* EXPORTCALL tesseract_GetThresholdedImage(tessHandle APIHandle);

extern void EXPORTCALL tesseract_Clear(tessHandle APIHandle);

extern void EXPORTCALL tesseract_ClearAdaptiveClassifier(tessHandle APIHandle);

extern bool EXPORTCALL tesseract_SetVariable(tessHandle APIHandle, const char* variable, const char* value);

extern void EXPORTCALL tesseract_SetPageSegMode(tessHandle APIHandle, int mode);

extern int EXPORTCALL tesseract_GetPageSegMode(tessHandle APIHandle);

extern void EXPORTCALL tesseract_SetAccuracyVSpeed(tessHandle APIHandle, int mode);

extern Boxa* EXPORTCALL tesseract_GetRegions(tessHandle APIHandle, Pixa** pixa);

extern Boxa* EXPORTCALL tesseract_GetTextLines(tessHandle APIHandle, Pixa** pixa, int** blockids);

extern Boxa* EXPORTCALL tesseract_GetWords(tessHandle APIHandle, Pixa** pixa);

extern char* EXPORTCALL tesseract_GetHOCRText(tessHandle APIHandle, int page_id);

extern char* EXPORTCALL tesseract_GetBoxText(tessHandle APIHandle, int page_number);

extern char* EXPORTCALL tesseract_GetUNLVText(tessHandle APIHandle);

extern void EXPORTCALL tesseract_DeleteString(char* pCstr);

extern int EXPORTCALL tesseract_MeanTextConf(tessHandle APIHandle);

extern int* EXPORTCALL tesseract_AllWordConfidences(tessHandle APIHandle);

extern void EXPORTCALL tesseract_DeleteWordConfidences(int* pconf);

#ifdef __cplusplus
}
#endif
#endif/*TESSINTF_H*/
 
