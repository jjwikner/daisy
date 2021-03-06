/** 3616133886 **/
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
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "daisySimDac_inst_static.h"

static long   __BITWISE_LOGIC_OP_TEMP_VAR__;int daisySimDac_DcFuncDerLoad(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
double	__tmpStkVar_9 = 0.0;
long	__tmpStkVar_8 = 0;
long	__tmpStkVar_7 = 0;
long	__tmpStkVar_6 = 0;
long	__tmpStkVar_5 = 0;
long	__tmpStkVar_4 = 0;
long	__tmpStkVar_3 = 0;
long	__tmpStkVar_2 = 0;
long	__tmpStkVar_1 = 0;


/*  Pre-Anal Load Code. */__tmpStkVar_9 = 0;
_V_Static_vout_200 = 0.0;

/*  C Code Every Code. */{ahdlDoSetLineNum ((long) 40);ahdlDoSetLineNum ((long) 40);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = __INT_TO_DBL((0L));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 41);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = __INT_TO_DBL((0L));(_vref_152) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 44);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_1 = (((getAcrossSolution1(_DATA_16))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (128L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 45);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_2 = (((getAcrossSolution1((*((long *) (instData + 20)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (64L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 46);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_3 = (((getAcrossSolution1((*((long *) (instData + 24)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (32L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 47);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_4 = (((getAcrossSolution1((*((long *) (instData + 28)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (16L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 48);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_5 = (((getAcrossSolution1((*((long *) (instData + 32)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (8L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 49);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_6 = (((getAcrossSolution1((*((long *) (instData + 36)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (4L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 50);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_7 = (((getAcrossSolution1((*((long *) (instData + 40)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (2L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 51);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_8 = (((getAcrossSolution1((*((long *) (instData + 44)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (1L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 52);(_V_Static_vout_200) += ahdlDoTransition (*(void **) (instData + 184), (__FLOAT_DIV(((((_vref_152) * (_outScaled_112)))), __INT_TO_DBL((256L)))), _tRiseFallDel_64, _tRiseFallDel_64, _tRiseFallDel_64, 0, 1, (double *) &__tmpStkVar_9);}ahdlDoSetLineNum ((long) 53);    ahdlInterSetAcrossQuan(      *(void **)(instData + 192),
       _vout_52,       *(long*)(instData + 0),       _vout_flow_188,      _V_Static_vout_200   ,       1.0    );
    return __msgSeverity__;
} /** end of daisySimDac_DcFuncDerLoad **/

int daisySimDac_DcFuncLoad(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
double	__tmpStkVar_9 = 0.0;
long	__tmpStkVar_8 = 0;
long	__tmpStkVar_7 = 0;
long	__tmpStkVar_6 = 0;
long	__tmpStkVar_5 = 0;
long	__tmpStkVar_4 = 0;
long	__tmpStkVar_3 = 0;
long	__tmpStkVar_2 = 0;
long	__tmpStkVar_1 = 0;


/*  Pre-Anal Load Code. */__tmpStkVar_9 = 0;
_V_Static_vout_200 = 0.0;

/*  C Code Every Code. */{ahdlDoSetLineNum ((long) 40);ahdlDoSetLineNum ((long) 40);(_outScaled_112) = __INT_TO_DBL((0L));ahdlDoSetLineNum ((long) 41);(_vref_152) = __INT_TO_DBL((0L));ahdlDoSetLineNum ((long) 44);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_1 = (((getAcrossSolution1(_DATA_16))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (128L) : (0L))))));ahdlDoSetLineNum ((long) 45);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_2 = (((getAcrossSolution1((*((long *) (instData + 20)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (64L) : (0L))))));ahdlDoSetLineNum ((long) 46);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_3 = (((getAcrossSolution1((*((long *) (instData + 24)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (32L) : (0L))))));ahdlDoSetLineNum ((long) 47);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_4 = (((getAcrossSolution1((*((long *) (instData + 28)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (16L) : (0L))))));ahdlDoSetLineNum ((long) 48);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_5 = (((getAcrossSolution1((*((long *) (instData + 32)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (8L) : (0L))))));ahdlDoSetLineNum ((long) 49);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_6 = (((getAcrossSolution1((*((long *) (instData + 36)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (4L) : (0L))))));ahdlDoSetLineNum ((long) 50);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_7 = (((getAcrossSolution1((*((long *) (instData + 40)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (2L) : (0L))))));ahdlDoSetLineNum ((long) 51);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_8 = (((getAcrossSolution1((*((long *) (instData + 44)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (1L) : (0L))))));ahdlDoSetLineNum ((long) 52);(_V_Static_vout_200) += ahdlDoTransition (*(void **) (instData + 184), (__FLOAT_DIV(((((_vref_152) * (_outScaled_112)))), __INT_TO_DBL((256L)))), _tRiseFallDel_64, _tRiseFallDel_64, _tRiseFallDel_64, 0, 1, (double *) &__tmpStkVar_9);}ahdlDoSetLineNum ((long) 53);    ahdlInterSetAcrossQuan(      *(void **)(instData + 192),
       _vout_52,       *(long*)(instData + 0),       _vout_flow_188,      _V_Static_vout_200   ,       1.0    );
    return __msgSeverity__;
} /** end of daisySimDac_DcFuncLoad **/

int daisySimDac_TranFuncDerLoad(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
double	__tmpStkVar_9 = 0.0;
long	__tmpStkVar_8 = 0;
long	__tmpStkVar_7 = 0;
long	__tmpStkVar_6 = 0;
long	__tmpStkVar_5 = 0;
long	__tmpStkVar_4 = 0;
long	__tmpStkVar_3 = 0;
long	__tmpStkVar_2 = 0;
long	__tmpStkVar_1 = 0;


/*  Pre-Anal Load Code. */__tmpStkVar_9 = 0;
_V_Static_vout_200 = 0.0;

/*  C Code Every Code. */{ahdlDoSetLineNum ((long) 40);ahdlDoSetLineNum ((long) 40);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = __INT_TO_DBL((0L));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 41);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = __INT_TO_DBL((0L));(_vref_152) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 44);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_1 = (((getAcrossSolution1(_DATA_16))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (128L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 45);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_2 = (((getAcrossSolution1((*((long *) (instData + 20)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (64L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 46);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_3 = (((getAcrossSolution1((*((long *) (instData + 24)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (32L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 47);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_4 = (((getAcrossSolution1((*((long *) (instData + 28)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (16L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 48);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_5 = (((getAcrossSolution1((*((long *) (instData + 32)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (8L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 49);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_6 = (((getAcrossSolution1((*((long *) (instData + 36)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (4L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 50);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_7 = (((getAcrossSolution1((*((long *) (instData + 40)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (2L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 51);{double __tempValueResultStatic;double __tempValueResultDynamic;
__tempValueResultStatic = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_8 = (((getAcrossSolution1((*((long *) (instData + 44)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (1L) : (0L))))));(_outScaled_112) = __tempValueResultStatic;}ahdlDoSetLineNum ((long) 52);(_V_Static_vout_200) += ahdlDoTransition (*(void **) (instData + 184), (__FLOAT_DIV(((((_vref_152) * (_outScaled_112)))), __INT_TO_DBL((256L)))), _tRiseFallDel_64, _tRiseFallDel_64, _tRiseFallDel_64, 0, 1, (double *) &__tmpStkVar_9);}ahdlDoSetLineNum ((long) 53);    ahdlInterSetAcrossQuan(      *(void **)(instData + 192),
       _vout_52,       *(long*)(instData + 0),       _vout_flow_188,      _V_Static_vout_200   ,       1.0    );
    return __msgSeverity__;
} /** end of daisySimDac_TranFuncDerLoad **/

int daisySimDac_TranFuncLoad(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
double	__tmpStkVar_9 = 0.0;
long	__tmpStkVar_8 = 0;
long	__tmpStkVar_7 = 0;
long	__tmpStkVar_6 = 0;
long	__tmpStkVar_5 = 0;
long	__tmpStkVar_4 = 0;
long	__tmpStkVar_3 = 0;
long	__tmpStkVar_2 = 0;
long	__tmpStkVar_1 = 0;


/*  Pre-Anal Load Code. */__tmpStkVar_9 = 0;
_V_Static_vout_200 = 0.0;

/*  C Code Every Code. */{ahdlDoSetLineNum ((long) 40);ahdlDoSetLineNum ((long) 40);(_outScaled_112) = __INT_TO_DBL((0L));ahdlDoSetLineNum ((long) 41);(_vref_152) = __INT_TO_DBL((0L));ahdlDoSetLineNum ((long) 44);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_1 = (((getAcrossSolution1(_DATA_16))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (128L) : (0L))))));ahdlDoSetLineNum ((long) 45);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_2 = (((getAcrossSolution1((*((long *) (instData + 20)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (64L) : (0L))))));ahdlDoSetLineNum ((long) 46);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_3 = (((getAcrossSolution1((*((long *) (instData + 24)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (32L) : (0L))))));ahdlDoSetLineNum ((long) 47);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_4 = (((getAcrossSolution1((*((long *) (instData + 28)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (16L) : (0L))))));ahdlDoSetLineNum ((long) 48);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_5 = (((getAcrossSolution1((*((long *) (instData + 32)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (8L) : (0L))))));ahdlDoSetLineNum ((long) 49);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_6 = (((getAcrossSolution1((*((long *) (instData + 36)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (4L) : (0L))))));ahdlDoSetLineNum ((long) 50);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_7 = (((getAcrossSolution1((*((long *) (instData + 40)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (2L) : (0L))))));ahdlDoSetLineNum ((long) 51);(_outScaled_112) = ((((_outScaled_112) + __INT_TO_DBL(((__tmpStkVar_8 = (((getAcrossSolution1((*((long *) (instData + 44)))))) > ((__FLOAT_DIV((_vLogicHigh_88), ((2.00000000000000000000e+00))))))) ? (1L) : (0L))))));ahdlDoSetLineNum ((long) 52);(_V_Static_vout_200) += ahdlDoTransition (*(void **) (instData + 184), (__FLOAT_DIV(((((_vref_152) * (_outScaled_112)))), __INT_TO_DBL((256L)))), _tRiseFallDel_64, _tRiseFallDel_64, _tRiseFallDel_64, 0, 1, (double *) &__tmpStkVar_9);}ahdlDoSetLineNum ((long) 53);    ahdlInterSetAcrossQuan(      *(void **)(instData + 192),
       _vout_52,       *(long*)(instData + 0),       _vout_flow_188,      _V_Static_vout_200   ,       1.0    );
    return __msgSeverity__;
} /** end of daisySimDac_TranFuncLoad **/

int daisySimDac_NoiseLoadBiasDep(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
double	__tmpStkVar_9 = 0.0;
long	__tmpStkVar_8 = 0;
long	__tmpStkVar_7 = 0;
long	__tmpStkVar_6 = 0;
long	__tmpStkVar_5 = 0;
long	__tmpStkVar_4 = 0;
long	__tmpStkVar_3 = 0;
long	__tmpStkVar_2 = 0;
long	__tmpStkVar_1 = 0;


    return __msgSeverity__;
} /** end of daisySimDac_NoiseLoadBiasDep **/

int daisySimDac_OpPointCalc(int anal_mode, char* instData, char* modelData, char** stringList) {

int __msgSeverity__ = 0;
    return __msgSeverity__;
} /** end of daisySimDac_OpPointCalc **/

