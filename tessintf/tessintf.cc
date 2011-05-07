#include "tessintf.h"
#include "/usr/local/include/tesseract/baseapi.h"
#include <stdio.h>
#include <string.h>
using namespace std;
using namespace tesseract;

extern "C"{

tessHandle EXPORTCALL tesseract_new(char* language){
TessBaseAPI* Tess = new TessBaseAPI;
if (Tess->Init(NULL, language, NULL, 0, 1)!=0){
  delete Tess;
  return NULL;
  }
else {
  return Tess;
  }
};

extern void EXPORTCALL tesseract_destroy(tessHandle APIHandle){
  APIHandle->End();
  delete APIHandle;  
}

extern int EXPORTCALL tesseract_init(tessHandle APIHandle, const char* datapath, const char* language,
           char **configs, int configs_size, bool configs_global_only){
return APIHandle->Init(datapath, language, configs, configs_size, configs_global_only);  
}

extern void EXPORTCALL tesseract_ReadConfigFile(tessHandle APIHandle, const char* filename, bool global_only){
  APIHandle->ReadConfigFile(filename, global_only);
}

extern void EXPORTCALL tesseract_SetImage(tessHandle APIHandle, const Pix* pix){
APIHandle->SetImage( pix );  
}

extern void EXPORTCALL tesseract_SetRectangle(tessHandle APIHandle, int left, int top, int width, int height){
APIHandle->SetRectangle(left, top, width, height);
}

extern char* EXPORTCALL tesseract_GetUTF8Text(tessHandle APIHandle){
   return APIHandle->GetUTF8Text();
}

extern Pix* EXPORTCALL tesseract_GetThresholdedImage(tessHandle APIHandle){
  return APIHandle->GetThresholdedImage();
}

extern char* EXPORTCALL tesseract_GetBoxText(tessHandle APIHandle, int page_number){
  return APIHandle->GetBoxText( page_number );
}

extern void EXPORTCALL tesseract_Clear(tessHandle APIHandle){
  APIHandle->Clear();
}

extern void EXPORTCALL tesseract_ClearAdaptiveClassifier(tessHandle APIHandle){
  APIHandle->ClearAdaptiveClassifier();
}


}

