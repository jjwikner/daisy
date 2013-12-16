/***********************************************************************************
 ** Copyright (c) 2003 Cadence Design Systems, Inc. All rights reserved.          **
 **                                                                               **
 ** This work may not be copied, modified, re-published, uploaded, executed, or   **
 ** distributed in any way, in any medium, whether in whole or in part, without   **
 ** prior written permission from Cadence Design Systems, Inc.                    **
 **                                                                               **
 ** This file is automatically generated and will be overwritten. The interfaces  **
 ** used within this generated code are subject to change without notice.         **
 ***********************************************************************************/
#include "ahdlcmi.h"

int ahdlBsourceInterfaceVersion() {
   return 1; /* Only used when version >= spectre's BsourceInterfaceVersion */
}

extern int daisySimDac_DcFuncDerLoad(int, char*, char*, char**);
extern int daisySimDac_DcFuncLoad(int, char*, char*, char**);
extern int daisySimDac_TranFuncDerLoad(int, char*, char*, char**);
extern int daisySimDac_TranFuncLoad(int, char*, char*, char**);
extern int daisySimDac_NoiseLoadBiasDep(int, char*, char*, char**);
extern int daisySimDac_OpPointCalc(int, char*, char*, char**);

int daisySimDac_InstallDeviceBindings(struct ahdlDevDescData *desc) {

   ahdlBindDevDescFuncs(desc,
                        daisySimDac_DcFuncDerLoad,  
                        daisySimDac_DcFuncLoad,     
                        daisySimDac_TranFuncDerLoad,
                        daisySimDac_TranFuncLoad,   
                        daisySimDac_NoiseLoadBiasDep,
                        daisySimDac_OpPointCalc
                       );
   return 1; /** success **/
}
extern int siconSimDAC_DcFuncDerLoad(int, char*, char*, char**);
extern int siconSimDAC_DcFuncLoad(int, char*, char*, char**);
extern int siconSimDAC_TranFuncDerLoad(int, char*, char*, char**);
extern int siconSimDAC_TranFuncLoad(int, char*, char*, char**);
extern int siconSimDAC_NoiseLoadBiasDep(int, char*, char*, char**);
extern int siconSimDAC_OpPointCalc(int, char*, char*, char**);

int siconSimDAC_InstallDeviceBindings(struct ahdlDevDescData *desc) {

   ahdlBindDevDescFuncs(desc,
                        siconSimDAC_DcFuncDerLoad,  
                        siconSimDAC_DcFuncLoad,     
                        siconSimDAC_TranFuncDerLoad,
                        siconSimDAC_TranFuncLoad,   
                        siconSimDAC_NoiseLoadBiasDep,
                        siconSimDAC_OpPointCalc
                       );
   return 1; /** success **/
}