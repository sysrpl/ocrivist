///////////////////////////////////////////////////////////////////////
// File:        tessintf.h
// Description: C interface to the tesseract API .
// Author:      Malcolm Poole
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
#ifndef TESSI_H
#define TESSINTF_H

#ifdef __cplusplus
#include "/usr/local/include/tesseract/baseapi.h"
#define EXPORTCALL __attribute__((stdcall))
typedef tesseract::TessBaseAPI *tessHandle;
#else
typedef int bool;
typedef struct tesseract_t *tessHandle;
typedef void* Pix ;
#define EXPORTCALL
#endif

#define DEFAULT_CRYPTO_SIZE 10240

#ifdef __cplusplus

extern "C"
{
#endif

typedef unsigned int U_Int;

extern tessHandle EXPORTCALL tesseract_new(char* language);

extern void EXPORTCALL tesseract_destroy(tessHandle APIHandle);

extern int EXPORTCALL tesseract_init(tessHandle APIHandle, const char* datapath, const char* language,
           char **configs, int configs_size, bool configs_global_only);
	   
extern void EXPORTCALL tesseract_ReadConfigFile(tessHandle APIHandle, const char* filename, bool global_only);

extern void EXPORTCALL tesseract_SetImage(tessHandle APIHandle, const Pix* pix);

extern void EXPORTCALL tesseract_SetRectangle(tessHandle APIHandle, int left, int top, int width, int height);

extern char* EXPORTCALL tesseract_GetUTF8Text(tessHandle APIHandle);

extern Pix* EXPORTCALL tesseract_GetThresholdedImage(tessHandle APIHandle);

extern char* EXPORTCALL tesseract_GetBoxText(tessHandle APIHandle, int page_number);

  /**
   * Free up recognition results and any stored image data, without actually
   * freeing any recognition data that would be time-consuming to reload.
   * Afterwards, you must call SetImage or TesseractRect before doing
   * any Recognize or Get* operation.
   */
extern void EXPORTCALL tesseract_Clear(tessHandle APIHandle);

  /**
   * Call between pages or documents etc to free up memory and forget
   * adaptive data.
   */
extern void EXPORTCALL tesseract_ClearAdaptiveClassifier(tessHandle APIHandle);


#ifdef __cplusplus
}
#endif
#endif/*TESSINTF_H*/
 
