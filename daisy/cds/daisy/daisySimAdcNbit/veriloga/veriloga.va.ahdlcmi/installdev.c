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

extern int daisySimAdcNbit_DcFuncDerLoad(int, char*, char*, char**);
extern int daisySimAdcNbit_DcFuncLoad(int, char*, char*, char**);
extern int daisySimAdcNbit_TranFuncDerLoad(int, char*, char*, char**);
extern int daisySimAdcNbit_TranFuncLoad(int, char*, char*, char**);
extern int daisySimAdcNbit_NoiseLoadBiasDep(int, char*, char*, char**);
extern int daisySimAdcNbit_OpPointCalc(int, char*, char*, char**);

int daisySimAdcNbit_InstallDeviceBindings(struct ahdlDevDescData *desc) {

   ahdlBindDevDescFuncs(desc,
                        daisySimAdcNbit_DcFuncDerLoad,  
                        daisySimAdcNbit_DcFuncLoad,     
                        daisySimAdcNbit_TranFuncDerLoad,
                        daisySimAdcNbit_TranFuncLoad,   
                        daisySimAdcNbit_NoiseLoadBiasDep,
                        daisySimAdcNbit_OpPointCalc
                       );
   return 1; /** success **/
}